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

#endif
