''debug helper
''used to regroup all necessary for internal debugging
''debug_int.bas

#if __FB_DEBUG__

#include once "fbint.bi"
#include once "parser.bi"
#include once "emit.bi"

#include once "fb.bi"
#include once "ir.bi"
#include once "reg.bi"
#include once "debug-int.bi"
#include once "emit-private.bi"
#include once "flist.bi"

common shared regTB() as REGCLASS ptr

'':::::
'' ppDump      =   '#'DUMP|ODUMP Expression
''
sub ppDumpTree _
	( _
		byval optimize as integer _
	)

	dim as ASTNODE ptr expr = any

	expr = cExpression( )

	if( expr <> NULL ) then

		if( optimize ) then
			expr = astOptimizeTree( expr )
		end if

		astDumpTree( expr )

		astDelTree( expr )
	else
		errReport( FB_ERRMSG_SYNTAXERROR )
	end if

end sub

'':::::
'' ppLookup    =   '#'LOOKUP name
''
sub ppLookup _
	( _
	)

	symbDumpLookup( lexGetText( ) )
	lexSkipToken( )

end sub

sub regDump2( byval this_ as REGCLASS ptr )
	for i as integer = 0 to this_->regs - 1
		print i & " " & emitDumpRegName( iif( this_->class = FB_DATACLASS_INTEGER, FB_DATATYPE_INTEGER, FB_DATATYPE_DOUBLE ), i );

		if( REG_ISUSED( this_->regctx.freeTB, i ) ) then
			print ", used";
		else
			print ", free";
		end if

		if( this_->vregTB(i) ) then
			print ", vreg=" & vregDumpToStr( this_->vregTB(i) );
		else
			print ", no vreg";
		end if
		if( this_->vauxparent(i) ) then
			print ", vauxparent=" & vregDumpToStr( this_->vauxparent(i) );
		end if
		print
	next
end sub

function emitDumpRegName( byval dtype as integer, byval reg as integer ) as string
	function = *hGetRegName( dtype, reg )
end function

sub hDumpFreeIntRegs( )
	dim as string free, used
	dim as integer reg = any

	'' For each register in the integer class
	reg = regTB(FB_DATACLASS_INTEGER)->getFirst( regTB(FB_DATACLASS_INTEGER) )
	while( reg <> INVALID )

		if( regTB(FB_DATACLASS_INTEGER)->isFree( regTB(FB_DATACLASS_INTEGER), reg ) ) then
			if( len( free ) > 0 ) then free += ", "
			free += emitDumpRegName( FB_DATATYPE_INTEGER, reg )
		else
			if( len( used ) > 0 ) then used += ", "
			used += emitDumpRegName( FB_DATATYPE_INTEGER, reg )
		end if

		reg = regTB(FB_DATACLASS_INTEGER)->getNext( regTB(FB_DATACLASS_INTEGER), reg )
	wend

	print , "used: " & used & " | free: " & free
end sub

private sub hDump _
	( _
		byval op as integer, _
		byval v1 as IRVREG ptr, _
		byval v2 as IRVREG ptr, _
		byval vr as IRVREG ptr, _
		byval wrapline as integer = FALSE _
	)

	dim s as string

	if( astGetOpId( op ) <> NULL ) then
		s = *astGetOpId( op )
	else
		s = str( op )
	end if

	const MAXLEN = 4
	select case( len( s ) )
	case is > MAXLEN
		s = left( s, MAXLEN )
	case is < MAXLEN
		s += space( MAXLEN - len( s ) )
	end select
	s = "[" + s + "]"

	#macro hDumpVr( id, v )
		if( v <> NULL ) then
			if( wrapline ) then
				s += !"\t"
			else
				s += " "
			end if
			s += id + " = " + vregDumpToStr( v )
			if( wrapline ) then
				s += NEWLINE
			else
				s += !"\t"
			end if
		end if
	#endmacro

	hDumpVr( "d", vr )
	hDumpVr( "l", v1 )
	hDumpVr( "r", v2 )

	if( wrapline = FALSE ) then
		s += NEWLINE
	end if

	if( (wrapline = FALSE) and (len( s ) > 79) ) then
		hDump( op, v1, v2, vr, TRUE )
	else
		print s;
	end if

end sub

function tacvregDump( byval tacvreg as IRTACVREG ptr ) as string
	if( tacvreg = NULL ) then
		return "<NULL>"
	end if
	function = "IRTACVREG( " & _
		"vreg=" & vregDumpToStr( tacvreg->vreg ) & ", " & _
		"parent=" & vregDumpToStr( tacvreg->parent ) & ", " & _
		"next=" & tacvregDump( tacvreg->next ) & " )"
end function

sub tacDump( byval tac as IRTAC ptr )
	if( tac = NULL ) then
		print "IRTAC: <NULL>"
		exit sub
	end if
	print "IRTAC: pos=" & tac->pos & ", op=" & tac->op
	print , "vr vreg: " & tacvregDump( @tac->vr.reg )
	print , "vr vidx: " & tacvregDump( @tac->vr.idx )
	print , "vr vaux: " & tacvregDump( @tac->vr.aux )
	print , "v1 vreg: " & tacvregDump( @tac->v1.reg )
	print , "v1 vidx: " & tacvregDump( @tac->v1.idx )
	print , "v1 vaux: " & tacvregDump( @tac->v1.aux )
	print , "v2 vreg: " & tacvregDump( @tac->v2.reg )
	print , "v2 vidx: " & tacvregDump( @tac->v2.idx )
	print , "v2 vaux: " & tacvregDump( @tac->v2.aux )
end sub

sub astDtorListDump( )
	dim as AST_DTORLIST_ITEM ptr i = any

	print "-------------- dtorlist: ------------------"
	i = listGetTail( @ast.dtorlist )
	while( i )
		if( i->cookie = -1 ) then
			print "    "; "*deleted*"; " cookie: ";i->cookie;" refcount: ";i->refcount
		else
			print "    ";symbDumpToStr( i->sym );" cookie: ";i->cookie;" refcount: ";i->refcount;" has dtor? ";hHasDtor( i->sym )
		end if
		i = listGetPrev( i )
	wend
end sub

function astDumpInline( byval n as ASTNODE ptr ) as string
	static reclevel as integer

	reclevel += 1

	dim s as string
	if( n = NULL ) then
		s = "<NULL>"
	else
		s += hAstNodeClassToStr( n->class )
		's += typeDump( n->dtype, n->subtype )

		var have_data = (n->sym <> NULL) or (n->l <> NULL) or (n->r <> NULL)
		select case as const( n->class )
		case AST_NODECLASS_BOP, AST_NODECLASS_UOP, AST_NODECLASS_CONST
			have_data or= TRUE
		end select

		if( have_data ) then
			s += "( "
		end if

		select case as const( n->class )
		case AST_NODECLASS_BOP, AST_NODECLASS_UOP
			s += astDumpOpToStr( n->op.op ) + ", "
		case AST_NODECLASS_CONST
			if( typeGetClass( n->dtype ) = FB_DATACLASS_FPOINT ) then
				s += str( astConstGetFloat( n ) ) + ", "
			else
				s += str( astConstGetInt( n ) ) + ", "
			end if
		end select

		if( n->sym ) then
			s += *symbGetName( n->sym ) + ", "
		end if
		if( n->l ) then
			s += astDumpInline( n->l ) + ", "
		end if
		if( n->r ) then
			s += astDumpInline( n->r ) + ", "
		end if

		if( have_data ) then
			if( right( s, 2 ) = ", " ) then
				s = left( s, len( s ) - 2 )
			end if
			s += " )"
		end if
	end if

	reclevel -= 1

	function = s
end function

sub astDumpSmall( byval n as ASTNODE ptr, byref prefix as string )
	static reclevel as integer

	reclevel += 1

	dim s as string

	'' Indentation
	s += space( (reclevel - 1) * 4 ) + prefix

	if( n = NULL ) then
		s += "<NULL>"
	else
		's += "[" + hex( n ) + "] "
		s += hAstNodeClassToStr( n->class )
		s += typeDumpToStr( n->dtype, n->subtype )

		select case as const( n->class )
		case AST_NODECLASS_MEM
			select case n->mem.op
			case AST_OP_MEMCLEAR
				s += " memclear"
			case AST_OP_MEMMOVE
				s += " memmove"
			end select
			s += " bytes=" & n->mem.bytes
		case AST_NODECLASS_VAR     : if( n->var_.ofs ) then s += " ofs=" & n->var_.ofs
		case AST_NODECLASS_DEREF   : if( n->ptr.ofs  ) then s += " ofs=" & n->ptr.ofs
		case AST_NODECLASS_OFFSET  : if( n->ofs.ofs  ) then s += " ofs=" & n->ofs.ofs
		case AST_NODECLASS_IDX     : if( n->idx.ofs  ) then s += " ofs=" & n->idx.ofs
			if( n->idx.mult <> 1 ) then s += " mult=" & n->idx.mult
		case AST_NODECLASS_BOP, AST_NODECLASS_UOP
			s += " " + astDumpOpToStr( n->op.op )
		case AST_NODECLASS_CONV
			if( n->cast.doconv = FALSE and n->cast.convconst = FALSE ) then
				s += " noconv"
			elseif( n->cast.doconv ) then
				s += " conv"
			elseif( n->cast.convconst ) then
				s += " convconst"
			end if
		case AST_NODECLASS_CONST
			if( typeGetClass( n->dtype ) = FB_DATACLASS_FPOINT ) then
				s += " " + str( astConstGetFloat( n ) )
			else
				s += " " + str( astConstGetInt( n ) )
			end if
		end select

		if( n->sym ) then
			#if 1
				s += " "
				if( symbIsProc( n->sym ) ) then
					s += symbMethodToStr( n->sym )
				elseif n->sym->id.name = NULL then
					s += *n->sym->id.alias
				else
					s += *n->sym->id.name
				end if
			#else
				s += " " + symbDumpToStr( n->sym )
			#endif
		end if
	end if

	print s

	if( n ) then
		if( n->l ) then
			astDumpSmall( n->l, "l: " )
		end if
		if( n->r ) then
			astDumpSmall( n->r, "r: " )
		end if
	end if

	reclevel -= 1
end sub

'':::::
private sub astDumpTreeEx _
	( _
		byval n as ASTNODE ptr, _
		byval col as integer, _
		byval just as integer, _
		byval depth as integer _
	)

	if( col <= 4 or col >= 76 ) then
		col = 40
	end if

	if( n = NULL ) then
		print "<NULL>"
		exit sub
	end if

	dim as string s
	's += "[" + hex( n, 8 ) + "] "
	s += hAstNodeToStr( n )
#if __FB_DEBUG__
	s += " " + typeDumpToStr( n->dtype, n->subtype )
#endif
	dbg_astOutput( s, col, just, depth )

	depth += 1

	if( n->l <> NULL ) then
		if( n->r <> NULL ) then
			dbg_astOutput( "/ \", col-2, 0 )
		else
			dbg_astOutput( "/", col-2, 0 )
		end if
	elseif( n->r <> NULL ) then
		dbg_astOutput( "  \", col-2, 0 )
	else
		dbg_astOutput( "", 0, 0 )
	end if

	if( n->l <> NULL ) then
		astDumpTreeEx( n->l, col-2, -1, depth )
	end if
	if( n->r <> NULL ) then
		astDumpTreeEx( n->r, col+2, 1, depth )
	end if

end sub

'':::::
sub astDumpTree _
	( _
		byval n as ASTNODE ptr, _
		byval col as integer _
	)

	astDumpTreeEx( n, col, -1, 0 )

end sub

''::::
sub astDumpList _
	( _
		byval n as ASTNODE ptr, _
		byval col as integer _
	)

	do while( n <> NULL )
		astDumpTree( n, col )
		n = n->next
	loop

end sub




#endif

#if (__FB_DEBUG__ <> 0) orelse defined(__GAS64_DEBUG__)
function vregDumpToStr( byval v as IRVREG ptr ) as string
	dim as string s
	dim as string regname

	if( v = NULL ) then
		return "<NULL>"
	end if

	static as zstring ptr vregtypes(IR_VREGTYPE_IMM to IR_VREGTYPE_OFS) = _
	{ _
		@"imm", @"var", @"idx", @"ptr", @"reg", @"ofs" _
	}

	#if 0
		s += "[" + hex( v, 8 ) + "] "
	#endif

	s += *vregtypes(v->typ)

	select case( v->typ )
	case IR_VREGTYPE_IMM
		s += " "
		if( typeGetClass( v->dtype ) = FB_DATACLASS_FPOINT ) then
			s += str( v->value.f )
		else
			s += str( v->value.i )
		end if

	case IR_VREGTYPE_REG
		if( env.clopt.backend = FB_BACKEND_GAS ) then
			regname = emitDumpRegName( v->dtype, v->reg )
			if( len( regname ) > 0 ) then
				s += " " + ucase( regname )
			else
				s += " " + str( v->reg )
			end if
		else
			''s += " reg="
			s += " " + str( v->reg )
		end if
	end select

	if( v->sym ) then
		s += " " + *symbGetName( v->sym )
	end if

	if( v->typ <> IR_VREGTYPE_REG ) then
		if( v->ofs ) then
			if( (env.clopt.backend = FB_BACKEND_GAS) and (v->sym <> NULL) ) then
				s += " [" + *symbGetMangledName( v->sym )
				if( v->ofs >= 0 ) then
					s += "+"
				end if
				s += str( v->ofs )
				s += "]"
			else
				s += " ofs=" + str( v->ofs )
			end if
		end if
		if( v->mult ) then
			s += " mult=" + str( v->mult )
		end if
	end if

	s += " " + typeDumpToStr( v->dtype, v->subtype )

	if( v->typ <> IR_VREGTYPE_REG ) then
		if( v->vidx ) then
			s += " vidx=<" + vregDumpToStr( v->vidx ) + ">"
		end if
	end if

	'' If it's a longint vreg, show vaux
	'' ASM backend: uses vaux, so always show it
	'' C/LLVM backends: don't use vaux, so only show it if it's set
	if( ISLONGINT( v->dtype ) and _
	    ((env.clopt.backend = FB_BACKEND_GAS) or (v->vaux <> NULL)) ) then
		s += " vaux=<" + vregDumpToStr( v->vaux ) + ">"
	end if

	function = s
end function

sub vregDump( byval v as IRVREG ptr )
	print vregDumpToStr( v )
end sub

static shared as zstring ptr classnames(FB_SYMBCLASS_VAR to FB_SYMBCLASS_NSIMPORT) = _
{ _
	@"var"      , _
	@"const"    , _
	@"proc"     , _
	@"param"    , _
	@"define"   , _
	@"keyword"  , _
	@"label"    , _
	@"namespace", _
	@"enum"     , _
	@"struct"   , _
	@"class"    , _
	@"field"    , _
	@"typedef"  , _
	@"fwdref"   , _
	@"scope"    , _
	@"reserved" , _
	@"nsimport"   _
}

'' For debugging
function typeDumpToStr _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval verbose as boolean _
	) as string

	dim as string dump
	dim as integer ok = any, ptrcount = any, dtypeonly = any

	dump = "["

	if( dtype and FB_DATATYPE_INVALID ) then
		dump += "invalid"
		ok = (subtype = NULL)
	else
		ptrcount = typeGetPtrCnt( dtype )
		assert( ptrcount >= 0 )

		if( typeIsRef( dtype ) ) then
			dump += "byref "
		end if

		if( typeIsConstAt( dtype, ptrcount ) ) then
			dump += "const "
		end if

		dtypeonly = typeGetDtOnly( dtype )
		select case( dtypeonly )
		case FB_DATATYPE_STRUCT
			dump += "struct"
		case FB_DATATYPE_WCHAR
			dump += "wchar"
		case FB_DATATYPE_FIXSTR
			dump += "fixstr"
		case else
			if( (dtypeonly >= 0) and (dtypeonly < FB_DATATYPES) ) then
				dump += *symb_dtypeTB(dtypeonly).name
			else
				dump += "<invalid dtype " & dtypeonly & ">"
			end if
		end select

		if( typeHasMangleDt( dtype ) ) then
			dump += " alias """

			select case typeGetMangleDt( dtype )
			case FB_DATATYPE_CHAR
				dump += "char"
			case FB_DATATYPE_INTEGER, FB_DATATYPE_UINT
				dump += "long"
			case FB_DATATYPE_VA_LIST
				dump += "va_list"
			case else
				dump += "<" & typeGetMangleDt( dtype ) & ">"
			end select

			dump += """"
		end if

		'' UDT name
		select case( typeGetDtOnly( dtype ) )
		case FB_DATATYPE_STRUCT, FB_DATATYPE_ENUM
			if( subtype ) then
				if( symbIsStruct( subtype ) ) then
					dump += " "
					dump += hGetNamespacePrefix( subtype )
					dump += *symbGetName( subtype )
				end if
			end if

		case FB_DATATYPE_NAMESPC
			if( subtype ) then
				if( symbIsNamespace( subtype ) ) then
					if( subtype = @symbGetGlobalNamespc( ) ) then
						dump += " <global namespace>"
					else
						dump += " " + *symbGetName( subtype )
					end if
				end if
			end if
		end select

		for i as integer = (ptrcount-1) to 0 step -1
			if( typeIsConstAt( dtype, i ) ) then
				dump += " const"
			end if
			dump += " ptr"
		next

		'' Report unusual subtypes
		if( subtype ) then
			select case( typeGetDtOnly( dtype ) )
			case FB_DATATYPE_STRUCT
				ok = symbIsStruct( subtype )
			case FB_DATATYPE_ENUM
				ok = symbIsEnum( subtype )
			case FB_DATATYPE_NAMESPC
				ok = symbIsNamespace( subtype )
			case FB_DATATYPE_FUNCTION
				ok = symbIsProc( subtype )
			case FB_DATATYPE_FWDREF
				ok = symbIsFwdref( subtype )
			case else
				ok = FALSE
			end select
		else
			select case( typeGetDtOnly( dtype ) )
			case FB_DATATYPE_STRUCT, FB_DATATYPE_ENUM, _
			     FB_DATATYPE_NAMESPC, _
			     FB_DATATYPE_FUNCTION, FB_DATATYPE_FWDREF
				ok = FALSE
			case else
				ok = TRUE
			end select
		end if
	end if

	if( ok = FALSE ) then
		dump += ", "
		if( subtype ) then
			if( (subtype->class >= FB_SYMBCLASS_VAR) and _
			    (subtype->class <  FB_SYMBCLASS_NSIMPORT) ) then
				dump += *classnames(subtype->class)
			else
				dump += str( subtype->class )
			end if
		else
			dump += "NULL"
		end if
	end if

	dump += "]"

	if( verbose ) then
		'' function pointer?
		if( typeGetDtOnly( dtype ) = FB_DATATYPE_FUNCTION ) then
			dump += "{" & symbDumpToStr( subtype, verbose ) & "}"
		end if
	end if

	function = dump
end function

sub typeDump( byval dtype as integer, byval subtype as FBSYMBOL ptr )
	print typeDumpToStr( dtype, subtype )
end sub

private sub hDumpName( byref s as string, byval sym as FBSYMBOL ptr )
	if( sym = @symbGetGlobalNamespc( ) ) then
		s += "<global namespace>"
	else
		s += hGetNamespacePrefix( sym )
	end if

	if( sym->id.name ) then
		s += *sym->id.name
	else
		s += "<unnamed>"
	end if

	if( sym->id.alias ) then
		s += " alias """ + *sym->id.alias + """"
	end if

#if 0
	'' Note: symbGetMangledName() will mangle the proc and set the
	'' "mangled" flag. If this is done too early though, before the proc is
	'' setup properly, then the mangled name will be empty or wrong.
	s += " mangled """ + *symbGetMangledName( sym ) + """"
#endif
end sub

function symbDumpToStr _
	( _
		byval sym as FBSYMBOL ptr, _
		byval verbose as boolean _
	) as string

	dim as string s

	if( sym = NULL ) then
		return "<NULL>"
	end if

#if 0
	s += "[" & hex( sym ) & "] "
#endif

#if 1
	if( (sym->class < FB_SYMBCLASS_VAR) or (sym->class > FB_SYMBCLASS_NSIMPORT) ) then
		s += "<bad class " + str( sym->class ) + "> "
	else
		s += *classnames(sym->class) + " "
	end if
#endif

#if 1
	#macro checkAttrib( ID )
		if( sym->attrib and FB_SYMBATTRIB_##ID ) then
			s += lcase( #ID ) + " "
		end if
	#endmacro

	checkAttrib( SHARED )
	checkAttrib( STATIC )
	checkAttrib( DYNAMIC )
	checkAttrib( COMMON )
	checkAttrib( EXTERN )
	checkAttrib( PUBLIC )
	checkAttrib( PRIVATE )
	checkAttrib( LOCAL )
	checkAttrib( EXPORT )
	checkAttrib( IMPORT )
	checkAttrib( INSTANCEPARAM )
	checkAttrib( PARAMVARBYDESC )
	checkAttrib( PARAMVARBYVAL )
	checkAttrib( PARAMVARBYREF )
	checkAttrib( LITERAL )
	checkAttrib( CONST )
	checkAttrib( TEMP )
	checkAttrib( DESCRIPTOR )
	checkAttrib( FUNCRESULT )
	checkAttrib( REF )
	checkAttrib( VIS_PRIVATE )
	checkAttrib( VIS_PROTECTED )
	checkAttrib( SUFFIXED )

	#macro checkPattrib( ID )
		if( sym->pattrib and FB_PROCATTRIB_##ID ) then
			s += lcase( #ID ) + " "
		end if
	#endmacro

	checkPAttrib( OVERLOADED )
	checkPAttrib( METHOD )
	checkPAttrib( CONSTRUCTOR )
	checkPAttrib( DESTRUCTOR1 )
	checkPAttrib( DESTRUCTOR0 )
	checkPAttrib( OPERATOR )
	checkPAttrib( PROPERTY )
	checkPAttrib( RETURNBYREF )
	checkPAttrib( STATICLOCALS )
	checkPAttrib( ABSTRACT )
	checkPAttrib( VIRTUAL )
	checkPAttrib( NOTHISCONSTNESS )
#endif

#if 1
	#macro checkStat( ID )
		if( sym->stats and FB_SYMBSTATS_##ID ) then
			s += lcase( #ID ) + " "
		end if
	#endmacro

	checkStat( ACCESSED )
	checkStat( CTORINITED )
	checkStat( DECLARED )
	checkStat( IMPLICIT )
	checkStat( RTL )
	checkStat( THROWABLE )
	checkStat( PARSED )
	checkStat( RTTITABLE )
	checkStat( HASALIAS )
	checkStat( VTABLE )
	if( symbIsProc( sym ) ) then
		checkStat( EXCLPARENT )
	else
		checkStat( DONTINIT )
	end if
	checkStat( MAINPROC )
	checkStat( MODLEVELPROC )
	checkStat( FUNCPTR )
	checkStat( JUMPTB )
	checkStat( GLOBALCTOR )
	checkStat( GLOBALDTOR )
	checkStat( CANTDUP )
	if( symbIsProc( sym ) ) then
		checkStat( CANBECLONED )
	else
		checkStat( ARGV )
	end if
	checkStat( HASRTTI )
	checkStat( CANTUNDEF )
	if( symbIsField( sym ) ) then
		checkStat( UNIONFIELD )
	elseif( symbIsProc( sym ) ) then
		checkStat( PROCEMITTED )
	else
		checkStat( WSTRING )
	end if

	checkStat( EMITTED )
	checkStat( BEINGEMITTED )
#endif

	if( sym->class = FB_SYMBCLASS_NSIMPORT ) then
		s += "from: "
		s += symbDumpToStr( sym->nsimp.imp_ns, verbose )
		return s
	end if

	select case( sym->class )
	case FB_SYMBCLASS_PROC
		s += *symbGetFullProcName( sym )

		select case( symbGetProcMode( sym ) )
		case FB_FUNCMODE_STDCALL    : s += " stdcall"
		case FB_FUNCMODE_STDCALL_MS : s += " stdcallms"
		case FB_FUNCMODE_PASCAL     : s += " pascal"
		case FB_FUNCMODE_CDECL      : s += " cdecl"
		case FB_FUNCMODE_THISCALL   : s += " thiscall"
		case FB_FUNCMODE_FASTCALL   : s += " fastcall"
		end select

		if( verbose ) then
			'' Dump parameters recursively (if any)
			s += "("
			var param = symbGetProcHeadParam( sym )
			while( param )
				s += symbDumpToStr( param, verbose )
				param = param->next
				if( param ) then
					s += ", "
				end if
			wend
			s += ")"
		end if

	case FB_SYMBCLASS_PARAM
		select case( symbGetParamMode( sym ) )
		case FB_PARAMMODE_BYVAL  : s += "byval "
		case FB_PARAMMODE_BYREF  : s += "byref "
		case FB_PARAMMODE_BYDESC : s += "bydesc "
		case FB_PARAMMODE_VARARG : s += "vararg "
		end select

		hDumpName( s, sym )

		if( sym->param.mode = FB_PARAMMODE_BYDESC ) then
			s += hDumpDynamicArrayDimensions( sym->param.bydescdimensions )
		end if

		if( sym->param.optexpr <> NULL ) then
			s += "?"
		end if


	case FB_SYMBCLASS_VAR, FB_SYMBCLASS_FIELD
		hDumpName( s, sym )

		'' Array dimensions, if any
		if( symbGetIsDynamic( sym ) ) then
			s += hDumpDynamicArrayDimensions( symbGetArrayDimensions( sym ) )
		elseif( symbGetArrayDimensions( sym ) > 0 ) then
			s += "("
			for i as integer = 0 to symbGetArrayDimensions( sym ) - 1
				if( i > 0 ) then
					s += ", "
				end if
				s &= symbArrayLbound( sym, i )
				s += " to "
				if( symbArrayUbound( sym, i ) = FB_ARRAYDIM_UNKNOWN ) then
					s += "..."
				else
					s &= symbArrayUbound( sym, i )
				end if
			next
			s += ")"
		end if

	case else
		hDumpName( s, sym )
	end select

	s += " "

	if( sym->typ and FB_DATATYPE_INVALID ) then
		if( sym->class = FB_SYMBCLASS_KEYWORD ) then
			s += "<keyword>"
		else
			s += "<invalid>"
		end if
	else
		'' UDTs themselves are FB_DATATYPE_STRUCT, but with NULL subtype,
		'' so treat that as special case, so symbTypeToStr() doesn't crash.
		if( sym->subtype = NULL ) then
			select case as const( sym->typ )
			case FB_DATATYPE_FWDREF
				s += "<fwdref>"
			case FB_DATATYPE_STRUCT
				if( symbIsStruct( sym ) ) then
					if( symbGetUDTIsUnion( sym ) ) then
						s += "<union>"
					else
						s += "<struct>"
					end if
				else
					s += "<struct>"
				end if
			case FB_DATATYPE_ENUM
				s += "<enum>"
			case else
				s += typeDumpToStr( sym->typ, NULL )
			end select
		else
			s += typeDumpToStr( sym->typ, sym->subtype, verbose )
		end if
	end if

	if( symbIsField( sym ) ) then
		if( sym->var_.bits > 0 ) then
			s += " bitfield : " & sym->var_.bits & " (" & sym->var_.bitpos & ".." & sym->var_.bitpos + sym->var_.bits - 1 & ")"
		end if

		s += " offset=" & sym->ofs
	end if

	function = s
end function

sub symbDump( byval sym as FBSYMBOL ptr, byval verbose as integer = 0 )
	print "symbDump [" + hex( sym ) + "]:"
	if( sym = NULL ) then
		exit sub
	end if
	print symbDumpToStr( sym, verbose )
end sub

sub symbDumpNamespace( byval ns as FBSYMBOL ptr )
	print "symbDumpNamespace [" + hex( ns ) + "]:"
	if( ns = NULL ) then
		exit sub
	endif
	select case( ns->class )
	case FB_SYMBCLASS_STRUCT, FB_SYMBCLASS_ENUM, FB_SYMBCLASS_NAMESPACE

	case else
		print "symbDumpNamespace(): not a namespace"
	end select

	print symbDumpToStr( ns ) + ":"

	var i = symbGetCompSymbTb( ns ).head
	while( i )
		print "    symtb: " + symbDumpToStr( i )
		i = i->next
	wend

	'' For each bucket in the hashtb...
	var hash = @symbGetCompHashTb( ns ).tb
	for index as integer = 0 to hash->nodes-1
		'' For each item in this bucket...
		'' (can have multiple items in case of hash collisions)
		var hashitem = hash->list[index].head
		while( hashitem )
			'' The user data stored in the hashtb entry is the "head" symbol.
			'' It can link to more symbols through its FBSYMBOL.hash.next field.
			'' symbNewSymbol() prepends new symbols to that list, so they shadow the previous ones.
			''   1st/head symbol = the one from the current scope
			''   other symbols   = shadowed symbols from parent scopes
			dim as FBSYMBOL ptr sym = hashitem->data
			var bucketprefix = "    hashtb[" & index & "]: "
			print bucketprefix + *hashitem->name + " = " + symbDumpToStr( sym )
			while( sym->hash.next )
				sym = sym->hash.next
				print space(len(bucketprefix)) + "next: " + symbDumpToStr( sym )
			wend
			hashitem = hashitem->next
		wend
	next
end sub

sub symbDumpChain( byval chain_ as FBSYMCHAIN ptr )
	print "symchain [" + hex( chain_ ) + "]:"
	if( chain_ ) then
		'' Also printing the "index" in the chain, so we can differentiate between
		'' symbols from the same FBSYMCHAIN node (linked by their FBSYMBOL.hash.next fields),
		'' and symbols in different FBSYMCHAIN nodes (linked via symbChainGetNext()).
		var i = 0
		do
			var sym = chain_->sym
			do
				print "   " & i & "  " + symbDumpToStr( sym )
				sym = sym->hash.next
			loop while( sym )
			i += 1
			chain_ = symbChainGetNext( chain_ )
		loop while( chain_ )
	end if
end sub

sub symbDumpLookup( byval id as zstring ptr )

	static as zstring * FB_MAXNAMELEN+1 sname
	'' hUcase( *id, sname )
	sname = *id
	id = @sname

	print "symbol: " & *id

	if( symbGetCurrentNamespc( ) <> NULL ) then
		print "namespace: " & symbDumpToStr( symbGetCurrentNamespc( ) )
	else
		print "global namespace"
	end if

	dim as uinteger index = hashHash( id )
	dim as FBHASHTB ptr hashtb = any
	hashtb = symb.hashlist.tail
	do
		dim as FBSYMBOL ptr sym = hashLookupEx( @hashtb->tb, id, index )
		while( sym )
			print symbDumpToStr( sym )
			sym = sym->hash.next
		wend
		hashtb = hashtb->prev
	loop while( hashtb <> NULL )

	dim as FBSYMCHAIN ptr imp_chain = hashLookupEx( @symb.imphashtb, id, index )
	symbDumpChain( imp_chain )

end sub

#endif

