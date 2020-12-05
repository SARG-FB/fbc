#if __FB_DEBUG__
'#include once "symb.bi.bi"

#include once "debug-internal.bi"
'#include once "reg.bi"

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


'':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'' dumping
'':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

'' The tables below use 'NameInfo' as the struct for
'' information. To keep the size down, the FullName
'' field and value field (unused anyway) are commented
'' out.

type NameInfo
	'' fullname as zstring ptr
	name as const zstring ptr
	'' value as integer
end type

'':::::
sub dbg_astOutput _
	( _
		byref s as string, _
		byval col as integer, _
		byval just as integer, _
		byval depth as integer = -1 _
	)

	dim pad as integer = any

	select case just
	case -1
		pad = col - len(s)
	case 1
		pad = col - 1
	case else
		pad = col
	end select

	if( depth < 0 ) then
		print space(pad-1); s
	else
		print str(depth); space(pad-1 - len(str(depth)) ); s
	end if

end sub

''
dim shared dbg_astNodeClassNames( 0 to AST_CLASSES-1 ) as NameInfo = _
{ _
	( /' @"AST_NODECLASS_NOP"              , '/ @"NOP"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_LOAD"             , '/ @"LOAD"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_ASSIGN"           , '/ @"ASSIGN"           /' , 0 '/ ), _
	( /' @"AST_NODECLASS_BOP"              , '/ @"BOP"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_UOP"              , '/ @"UOP"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_CONV"             , '/ @"CONV"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_ADDROF"           , '/ @"ADDROF"           /' , 0 '/ ), _
	( /' @"AST_NODECLASS_BRANCH"           , '/ @"BRANCH"           /' , 0 '/ ), _
	( /' @"AST_NODECLASS_JMPTB"            , '/ @"JMPTB"            /' , 0 '/ ), _
	( /' @"AST_NODECLASS_CALL"             , '/ @"CALL"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_CALLCTOR"         , '/ @"CALLCTOR"         /' , 0 '/ ), _
	( /' @"AST_NODECLASS_STACK"            , '/ @"STACK"            /' , 0 '/ ), _
	( /' @"AST_NODECLASS_MEM"              , '/ @"MEM"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_LOOP"             , '/ @"LOOP"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_COMP"             , '/ @"COMP"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_LINK"             , '/ @"LINK"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_CONST"            , '/ @"CONST"            /' , 0 '/ ), _
	( /' @"AST_NODECLASS_VAR"              , '/ @"VAR"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_IDX"              , '/ @"IDX"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_FIELD"            , '/ @"FIELD"            /' , 0 '/ ), _
	( /' @"AST_NODECLASS_DEREF"            , '/ @"DEREF"            /' , 0 '/ ), _
	( /' @"AST_NODECLASS_LABEL"            , '/ @"LABEL"            /' , 0 '/ ), _
	( /' @"AST_NODECLASS_ARG"              , '/ @"ARG"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_OFFSET"           , '/ @"OFFSET"           /' , 0 '/ ), _
	( /' @"AST_NODECLASS_DECL"             , '/ @"DECL"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_NIDXARRAY"        , '/ @"NIDXARRAY"        /' , 0 '/ ), _
	( /' @"AST_NODECLASS_IIF"              , '/ @"IIF"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_LIT"              , '/ @"LIT"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_ASM"              , '/ @"ASM"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_DATASTMT"         , '/ @"DATASTMT"         /' , 0 '/ ), _
	( /' @"AST_NODECLASS_DBG"              , '/ @"DBG"              /' , 0 '/ ), _
	( /' @"AST_NODECLASS_BOUNDCHK"         , '/ @"BOUNDCHK"         /' , 0 '/ ), _
	( /' @"AST_NODECLASS_PTRCHK"           , '/ @"PTRCHK"           /' , 0 '/ ), _
	( /' @"AST_NODECLASS_SCOPEBEGIN"       , '/ @"SCOPEBEGIN"       /' , 0 '/ ), _
	( /' @"AST_NODECLASS_SCOPEEND"         , '/ @"SCOPEEND"         /' , 0 '/ ), _
	( /' @"AST_NODECLASS_SCOPE_BREAK"      , '/ @"SCOPE_BREAK"      /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI"          , '/ @"TYPEINI"          /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI_PAD"      , '/ @"TYPEINI_PAD"      /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI_ASSIGN"   , '/ @"TYPEINI_ASSIGN"   /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI_CTORCALL" , '/ @"TYPEINI_CTORCALL" /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI_CTORLIST" , '/ @"TYPEINI_CTORLIST" /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI_SCOPEINI" , '/ @"TYPEINI_SCOPEINI" /' , 0 '/ ), _
	( /' @"AST_NODECLASS_TYPEINI_SCOPEEND" , '/ @"TYPEINI_SCOPEEND" /' , 0 '/ ), _
	( /' @"AST_NODECLASS_PROC"             , '/ @"PROC"             /' , 0 '/ ), _
	( /' @"AST_NODECLASS_MACRO"            , '/ @"MACRO"            /' , 0 '/ )  _
}

''
dim shared dbg_astNodeOpNames( 0 to AST_OPCODES - 1 ) as NameInfo = _
{ _
	( /' @"AST_OP_ASSIGN"          , '/ @"="            /' , 0 '/ ), _
	( /' @"AST_OP_ADD_SELF"        , '/ @"+="           /' , 0 '/ ), _
	( /' @"AST_OP_SUB_SELF"        , '/ @"-="           /' , 0 '/ ), _
	( /' @"AST_OP_MUL_SELF"        , '/ @"*="           /' , 0 '/ ), _
	( /' @"AST_OP_DIV_SELF"        , '/ @"/="           /' , 0 '/ ), _
	( /' @"AST_OP_INTDIV_SELF"     , '/ @"\="           /' , 0 '/ ), _
	( /' @"AST_OP_MOD_SELF"        , '/ @"MOD="         /' , 0 '/ ), _
	( /' @"AST_OP_AND_SELF"        , '/ @"AND="         /' , 0 '/ ), _
	( /' @"AST_OP_OR_SELF"         , '/ @"OR="          /' , 0 '/ ), _
	( /' @"AST_OP_ANDALSO_SELF"    , '/ @"ANDALSO="     /' , 0 '/ ), _
	( /' @"AST_OP_ORELSE_SELF"     , '/ @"ORELSE="      /' , 0 '/ ), _
	( /' @"AST_OP_XOR_SELF"        , '/ @"XOR="         /' , 0 '/ ), _
	( /' @"AST_OP_EQV_SELF"        , '/ @"EQV="         /' , 0 '/ ), _
	( /' @"AST_OP_IMP_SELF"        , '/ @"IMP="         /' , 0 '/ ), _
	( /' @"AST_OP_SHL_SELF"        , '/ @"SHL="         /' , 0 '/ ), _
	( /' @"AST_OP_SHR_SELF"        , '/ @"SHR="         /' , 0 '/ ), _
	( /' @"AST_OP_POW_SELF"        , '/ @"^="           /' , 0 '/ ), _
	( /' @"AST_OP_CONCAT_SELF"     , '/ @"&="           /' , 0 '/ ), _
	( /' @"AST_OP_NEW_SELF"        , '/ @"new="         /' , 0 '/ ), _
	( /' @"AST_OP_NEW_VEC_SELF"    , '/ @"new[]="       /' , 0 '/ ), _
	( /' @"AST_OP_DEL_SELF"        , '/ @"del="         /' , 0 '/ ), _
	( /' @"AST_OP_DEL_VEC_SELF"    , '/ @"del[]="       /' , 0 '/ ), _
	( /' @"AST_OP_ADDROF"          , '/ @"ADDROF"       /' , 0 '/ ), _
	( /' @"AST_OP_PTRINDEX"        , '/ @"PTRINDEX"     /' , 0 '/ ), _
	( /' @"AST_OP_FOR"             , '/ @"FOR"          /' , 0 '/ ), _
	( /' @"AST_OP_STEP"            , '/ @"STEP"         /' , 0 '/ ), _
	( /' @"AST_OP_NEXT"            , '/ @"NEXT"         /' , 0 '/ ), _
	( /' @"AST_OP_CAST"            , '/ @"CAST"         /' , 0 '/ ), _
	( /' @"AST_OP_ADD"             , '/ @"+"            /' , 0 '/ ), _
	( /' @"AST_OP_SUB"             , '/ @"-"            /' , 0 '/ ), _
	( /' @"AST_OP_MUL"             , '/ @"*"            /' , 0 '/ ), _
	( /' @"AST_OP_DIV"             , '/ @"/"            /' , 0 '/ ), _
	( /' @"AST_OP_INTDIV"          , '/ @"\"            /' , 0 '/ ), _
	( /' @"AST_OP_MOD"             , '/ @"MOD"          /' , 0 '/ ), _
	( /' @"AST_OP_AND"             , '/ @"AND"          /' , 0 '/ ), _
	( /' @"AST_OP_OR"              , '/ @"OR"           /' , 0 '/ ), _
	( /' @"AST_OP_ANDALSO"         , '/ @"ANDALSO"      /' , 0 '/ ), _
	( /' @"AST_OP_ORELSE"          , '/ @"ORELSE"       /' , 0 '/ ), _
	( /' @"AST_OP_XOR"             , '/ @"XOR"          /' , 0 '/ ), _
	( /' @"AST_OP_EQV"             , '/ @"EQV"          /' , 0 '/ ), _
	( /' @"AST_OP_IMP"             , '/ @"IMP"          /' , 0 '/ ), _
	( /' @"AST_OP_SHL"             , '/ @"SHL"          /' , 0 '/ ), _
	( /' @"AST_OP_SHR"             , '/ @"SHR"          /' , 0 '/ ), _
	( /' @"AST_OP_POW"             , '/ @"^"            /' , 0 '/ ), _
	( /' @"AST_OP_CONCAT"          , '/ @"&"            /' , 0 '/ ), _
	( /' @"AST_OP_EQ"              , '/ @"=="           /' , 0 '/ ), _
	( /' @"AST_OP_GT"              , '/ @">"            /' , 0 '/ ), _
	( /' @"AST_OP_LT"              , '/ @"<"            /' , 0 '/ ), _
	( /' @"AST_OP_NE"              , '/ @"<>"           /' , 0 '/ ), _
	( /' @"AST_OP_GE"              , '/ @">="           /' , 0 '/ ), _
	( /' @"AST_OP_LE"              , '/ @"<="           /' , 0 '/ ), _
	( /' @"AST_OP_IS"              , '/ @"IS"           /' , 0 '/ ), _
	( /' @"AST_OP_NOT"             , '/ @"NOT"          /' , 0 '/ ), _
	( /' @"AST_OP_PLUS"            , '/ @"+"            /' , 0 '/ ), _
	( /' @"AST_OP_NEG"             , '/ @"NEG"          /' , 0 '/ ), _
	( /' @"AST_OP_HADD"            , '/ @"HADD"         /' , 0 '/ ), _
	( /' @"AST_OP_ABS"             , '/ @"ABS"          /' , 0 '/ ), _
	( /' @"AST_OP_SGN"             , '/ @"SGN"          /' , 0 '/ ), _
	( /' @"AST_OP_SIN"             , '/ @"SIN"          /' , 0 '/ ), _
	( /' @"AST_OP_ASIN"            , '/ @"ASIN"         /' , 0 '/ ), _
	( /' @"AST_OP_COS"             , '/ @"COS"          /' , 0 '/ ), _
	( /' @"AST_OP_ACOS"            , '/ @"ACOS"         /' , 0 '/ ), _
	( /' @"AST_OP_TAN"             , '/ @"TAN"          /' , 0 '/ ), _
	( /' @"AST_OP_ATAN"            , '/ @"ATAN"         /' , 0 '/ ), _
	( /' @"AST_OP_ATAN2"           , '/ @"ATAN2"        /' , 0 '/ ), _
	( /' @"AST_OP_SQRT"            , '/ @"SQRT"         /' , 0 '/ ), _
	( /' @"AST_OP_RSQRT"           , '/ @"RSQRT"        /' , 0 '/ ), _
	( /' @"AST_OP_RCP"             , '/ @"RCP"          /' , 0 '/ ), _
	( /' @"AST_OP_LOG"             , '/ @"LOG"          /' , 0 '/ ), _
	( /' @"AST_OP_EXP"             , '/ @"EXP"          /' , 0 '/ ), _
	( /' @"AST_OP_FLOOR"           , '/ @"FLOOR"        /' , 0 '/ ), _
	( /' @"AST_OP_FIX"             , '/ @"FIX"          /' , 0 '/ ), _
	( /' @"AST_OP_FRAC"            , '/ @"FRAC"         /' , 0 '/ ), _
	( /' @"AST_OP_LEN"             , '/ @"LEN"          /' , 0 '/ ), _
	( /' @"AST_OP_CONVFD2FS"       , '/ @"CONVFD2FS"    /' , 0 '/ ), _
	( /' @"AST_OP_SWZREP"          , '/ @"SWZREP"       /' , 0 '/ ), _
	( /' @"AST_OP_DEREF"           , '/ @"DEREF"        /' , 0 '/ ), _
	( /' @"AST_OP_FLDDEREF"        , '/ @"FLDDEREF"     /' , 0 '/ ), _
	( /' @"AST_OP_NEW"             , '/ @"NEW"          /' , 0 '/ ), _
	( /' @"AST_OP_NEW_VEC"         , '/ @"NEW_VEC"      /' , 0 '/ ), _
	( /' @"AST_OP_DEL"             , '/ @"DEL"          /' , 0 '/ ), _
	( /' @"AST_OP_DEL_VEC"         , '/ @"DEL_VEC"      /' , 0 '/ ), _
	( /' @"AST_OP_TOINT"           , '/ @"TOINT"        /' , 0 '/ ), _
	( /' @"AST_OP_TOFLT"           , '/ @"TOFLT"        /' , 0 '/ ), _
	( /' @"AST_OP_TOBOOL"          , '/ @"TOBOOL"       /' , 0 '/ ), _
	( /' @"AST_OP_LOAD"            , '/ @"LOAD"         /' , 0 '/ ), _
	( /' @"AST_OP_LOADRES"         , '/ @"LOADRES"      /' , 0 '/ ), _
	( /' @"AST_OP_SPILLREGS"       , '/ @"SPILLREGS"    /' , 0 '/ ), _
	( /' @"AST_OP_PUSH"            , '/ @"PUSH"         /' , 0 '/ ), _
	( /' @"AST_OP_POP"             , '/ @"POP"          /' , 0 '/ ), _
	( /' @"AST_OP_PUSHUDT"         , '/ @"PUSHUDT"      /' , 0 '/ ), _
	( /' @"AST_OP_STACKALIGN"      , '/ @"STACKALIGN"   /' , 0 '/ ), _
	( /' @"AST_OP_JEQ"             , '/ @"JEQ"          /' , 0 '/ ), _
	( /' @"AST_OP_JGT"             , '/ @"JGT"          /' , 0 '/ ), _
	( /' @"AST_OP_JLT"             , '/ @"JLT"          /' , 0 '/ ), _
	( /' @"AST_OP_JNE"             , '/ @"JNE"          /' , 0 '/ ), _
	( /' @"AST_OP_JGE"             , '/ @"JGE"          /' , 0 '/ ), _
	( /' @"AST_OP_JLE"             , '/ @"JLE"          /' , 0 '/ ), _
	( /' @"AST_OP_JMP"             , '/ @"JMP"          /' , 0 '/ ), _
	( /' @"AST_OP_CALL"            , '/ @"CALL"         /' , 0 '/ ), _
	( /' @"AST_OP_LABEL"           , '/ @"LABEL"        /' , 0 '/ ), _
	( /' @"AST_OP_RET"             , '/ @"RET"          /' , 0 '/ ), _
	( /' @"AST_OP_CALLFUNCT"       , '/ @"CALLFUNCT"    /' , 0 '/ ), _
	( /' @"AST_OP_CALLPTR"         , '/ @"CALLPTR"      /' , 0 '/ ), _
	( /' @"AST_OP_JUMPPTR"         , '/ @"JUMPPTR"      /' , 0 '/ ), _
	( /' @"AST_OP_MEMMOVE"         , '/ @"MEMMOVE"      /' , 0 '/ ), _
	( /' @"AST_OP_MEMSWAP"         , '/ @"MEMSWAP"      /' , 0 '/ ), _
	( /' @"AST_OP_MEMCLEAR"        , '/ @"MEMCLEAR"     /' , 0 '/ ), _
	( /' @"AST_OP_STKCLEAR"        , '/ @"STKCLEAR"     /' , 0 '/ ), _
	( /' @"AST_OP_VA_START"        , '/ @"VA_START"     /' , 0 '/ ), _
	( /' @"AST_OP_VA_END"          , '/ @"VA_END"       /' , 0 '/ ), _
	( /' @"AST_OP_VA_COPY"         , '/ @"VA_COPY"      /' , 0 '/ ), _
	( /' @"AST_OP_VA_ARG"          , '/ @"VA_ARG"       /' , 0 '/ ), _
	( /' @"AST_OP_DBG_LINEINI"     , '/ @"DBG_LINEINI"  /' , 0 '/ ), _
	( /' @"AST_OP_DBG_LINEEND"     , '/ @"DBG_LINEEND"  /' , 0 '/ ), _
	( /' @"AST_OP_DBG_SCOPEINI"    , '/ @"DBG_SCOPEINI" /' , 0 '/ ), _
	( /' @"AST_OP_DBG_SCOPEEND"    , '/ @"BDG_SCOPEEND" /' , 0 '/ ), _
	( /' @"AST_OP_LIT_COMMENT"     , '/ @"LIT_COMMENT"  /' , 0 '/ ), _
	( /' @"AST_OP_LIT_ASM"         , '/ @"LIT_ASM"      /' , 0 '/ ), _
	( /' @"AST_OP_TOSIGNED"        , '/ @"TOSIGNED"     /' , 0 '/ ), _
	( /' @"AST_OP_TOUNSIGNED"      , '/ @"TOUNSIGNED"   /' , 0 '/ ) _
}

function astDumpOpToStr( byval op as AST_OP ) as string
	if(( op > AST_OPCODES - 1 ) or ( op < 0 )) then
		return "OP:" + str(op)
	end if
	return *dbg_astNodeOpNames( op ).name
end function

'':::::
function hAstNodeClassToStr _
	( _
		byval c as AST_NODECLASS _
	) as string

	if(( c > AST_CLASSES - 1 ) or ( c < 0 )) then
		return "CLASS:" + str(c)
	end if

	return *dbg_astNodeClassNames( c ).name

end function

'':::::
function hSymbToStr _
	( _
		byval s as FBSYMBOL ptr _
	) as string

	if( s = NULL ) then return ""

	if( s->id.name ) then
		return *(s->id.name)
	elseif( s->id.alias ) then
		return *(s->id.alias)
	end if
end function

'':::::
function hAstNodeTypeToStr _
	( _
		byval n as ASTNODE ptr _
	) as string

	if( n = NULL ) then return ""

	return symbTypeToStr( n->dtype, n->subtype )

end function

'':::::
function hAstNodeToStr _
	( _
		byval n as ASTNODE ptr _
	) as string

	#define NODE_TYPE ( " (" & hAstNodeTypeToStr( n ) & ")" )

	select case as const n->class
	case AST_NODECLASS_BOP
		return astDumpOpToStr( n->op.op ) & " =-= " & hSymbToStr( n->op.ex )

	case AST_NODECLASS_UOP
		return astDumpOpToStr( n->op.op )

	case AST_NODECLASS_CONST
		if( typeGetClass( n->dtype ) = FB_DATACLASS_FPOINT ) then
			return str( astConstGetFloat( n ) ) & NODE_TYPE
		end if
		return str( astConstGetInt( n ) ) & NODE_TYPE

	case AST_NODECLASS_VAR
		return "VAR( " & *iif( n->sym, symbGetName( n->sym ), @"<NULL>" ) & " )" & NODE_TYPE

	case AST_NODECLASS_FIELD
		return "FIELD( " & *symbGetName( n->sym ) & " )" & NODE_TYPE

	case AST_NODECLASS_DECL
		if( n->sym ) then
			return "DECL( " & *symbGetName( n->sym ) & " )"
		end if
		return "DECL" & NODE_TYPE

	case AST_NODECLASS_CALL
		return "CALL( " & *symbGetName( n->sym ) & " )"

	case AST_NODECLASS_LABEL
		return "LABEL: " & hSymbToStr( n->sym )

	case AST_NODECLASS_BRANCH
		return "BRANCH: " & astDumpOpToStr( n->op.op ) & " " & hSymbToStr( n->op.ex )

	case AST_NODECLASS_SCOPEBEGIN
		return "SCOPEBEGIN: " & hSymbToStr( n->sym )

	case AST_NODECLASS_TYPEINI_ASSIGN
		return "TYPEINI_ASSIGN( offset=" & n->typeini.ofs & " )"

	case AST_NODECLASS_MACRO
		return "MACRO: " & astDumpOpToStr( n->op.op ) & " " & NODE_TYPE

	case else
		return hAstNodeClassToStr( n->class ) & NODE_TYPE
	end select

end function

'':::::
sub astDumpTreeEx _
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

	s += " " + typeDumpToStr( n->dtype, n->subtype )

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

sub astDtorListDump( )
	dim as AST_DTORLIST_ITEM ptr i = any

	print "-------------- dtorlist: ------------------"
	i = listGetTail( @ast.dtorlist )
	while( i )
		print "    ";symbDumpToStr( i->sym );" cookie: ";i->cookie;" refcount: ";i->refcount;" has dtor? ";hHasDtor( i->sym )
		i = listGetPrev( i )
	wend
end sub

function hNodeIsFromCurrentProc( byval n as ASTNODE ptr ) as integer
	dim as ASTNODE ptr i = any

	i = ast.proc.curr->l
	while( i )

		if( i = n ) then
			function = TRUE
			exit while
		end if

		i = i->next
	wend
end function

'' Count the bitfield FIELD nodes in a tree
function astCountBitfields( byval n as ASTNODE ptr ) as integer
	dim as integer count = any

	count = 0

	if( n ) then
		if( astIsBITFIELD( n ) ) then
			count += 1
		end if

		count += astCountBitfields( n->l )
		count += astCountBitfields( n->r )
	end if

	function = count
end function

'' Count the TYPEINI nodes in a tree
function astCountTypeinis( byval n as ASTNODE ptr ) as integer
	dim as integer count = any

	count = 0

	if( n ) then
		if( astIsTYPEINI( n ) ) then
			count += 1
		end if

		count += astCountTypeinis( n->l )
		count += astCountTypeinis( n->r )
	end if

	function = count
end function

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

sub hDumpName( byref s as string, byval sym as FBSYMBOL ptr )
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
	checkAttrib( PARAMBYDESC )
	checkAttrib( PARAMBYVAL )
	checkAttrib( PARAMBYREF )
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
	checkPAttrib( DESTRUCTOR )
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

sub symbDump( byval sym as FBSYMBOL ptr )
	print symbDumpToStr( sym )
end sub

sub symbDumpNamespace( byval ns as FBSYMBOL ptr )
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


dim shared as zstring ptr classnamesPretty(FB_SYMBCLASS_VAR to FB_SYMBCLASS_NSIMPORT) = _
{ _
	@"variable", _
	@"constant", _
	@"procedure", _
	@"parameter", _
	@"#define", _
	@"keyword", _
	@"label", _
	@"namespace", _
	@"enum", _
	@"type", _
	@"class", _
	@"field", _
	@"type alias", _
	@"forward reference", _
	@"scope", _
	@"namespace import" _
}

function symbDumpPrettyToStr( byval sym as FBSYMBOL ptr ) as string
	function = *classnamesPretty(sym->class) + " " + *sym->id.name
end function



function emitDumpRegName( byval dtype as integer, byval reg as integer ) as string
	function = *hGetRegName( dtype, reg )
end function



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

#if __FB_DEBUG__ <> 0
function hemittype _
	( _
	byval dtype as integer, _
	byval subtype as FBSYMBOL ptr _
	) as string

	dim as string s
	dim as integer ptrcount = any

	ptrcount = typeGetPtrCnt( dtype )
	dtype = typeGetDtOnly( dtype )

	select case as const( dtype )
		case FB_DATATYPE_VOID
			'' "void*" isn't allowed in L L V M IR, "i8*" must be used instead,
			'' that's why FB_DATATYPE_VOID is mapped to "i8" in the above
			'' table. "void" can only be used for subs.
			if( ptrcount = 0 ) then
				s = "[void]"
			else
				s = "[void ptr]"
			end if

		case FB_DATATYPE_STRUCT, FB_DATATYPE_ENUM
			if( subtype ) then
				hEmitUDT2( subtype )
				s = *symbGetMangledName( subtype )
			elseif( dtype = FB_DATATYPE_ENUM ) then
				s = "[enum=integer]"
			else
				s = "[void]"
			end if

		case FB_DATATYPE_FUNCTION
			assert( ptrcount > 0 )
			ptrcount -= 1
			s="datatype function ptr ="+*symbGetMangledName(subtype)+ "*"
		case FB_DATATYPE_CHAR, FB_DATATYPE_WCHAR
			'' Emit ubyte instead of char,
			'' and ubyte/ushort/uinteger instead of wchar_t
			s = "[ubyte or ushort or uinteger]"

		case FB_DATATYPE_FIXSTR
			'' Ditto (but typeGetRemapType() returns FB_DATATYPE_FIXSTR,
			'' so do it manually)
			s = "[ubyte]"

		case else
			s = typedumpToStr(dtype,0)
	end select

	if( ptrcount > 0 ) then
		s += string( ptrcount, "*" )
	end if

	function = s
end function
function vregdumpfull( byval v as IRVREG ptr ) as string
	return vregDumpToStr(v)+iif(v<>0," symbdump="+symbdumpToStr(v->sym),"")
end function
function vregpretty( byval v as IRVREG ptr ) as string
	dim s as string
	select case( v->typ )
		case IR_VREGTYPE_IMM
			if( typeGetClass( v->dtype ) = FB_DATACLASS_FPOINT ) then
				s = str( v->value.f )
			else
				s = str( v->value.i )
			end if

		case IR_VREGTYPE_REG
			if( v->sym ) then
				s = *symbGetMangledName( v->sym )
			else
				s = "vr" & v->reg
			end if

		case else
			if( v->sym ) then
				s = *symbGetMangledName( v->sym )
			end if
	end select

	if( v->vidx ) then
		if( len( s ) > 0 ) then
			s += "+"
		end if
		s += vregPretty( v->vidx )
	end if
	if( v->ofs ) then
		s += "+" & v->ofs
	end if
	if( v->mult ) then
		s += " (mult) *" & v->mult
	end if

	s += " " + typedumpToStr( v->dtype, v->subtype )

	function = s
end function

function hdumpProcHeader(byval proc as FBSYMBOL ptr) as string

	dim as string ln
	dim as integer dtype = any
	dim as FBSYMBOL ptr subtype = Any



	'' function result type (is 'void' for subs)
	ln=typedumpToStr(typeGetDtAndPtrOnly( symbGetProcRealType( proc ) ), symbGetProcRealSubtype( proc ) )
	ln += " "+*symbGetMangledName( proc )

	'' Parameter list
	ln += " ( "

	'' if returning a struct, there's an extra parameter
	dim as FBSYMBOL ptr hidden = NULL
	if( symbProcreturnsOnStack( proc ) ) then
		hidden = proc->proc.ext->res
		'asm_info("hidden")
		if hidden<>0 then
			ln += hEmitType( typeAddrOf( symbGetType( hidden ) ), symbGetSubtype( hidden ) )
			ln+=" / "+typedumpToStr( typeAddrOf( symbGetType( hidden ) ), symbGetSubtype( hidden ))
			ln += " " + *symbGetMangledName( hidden ) + "$"
		end if

		if( symbGetProcParams( proc ) > 0 ) then
			ln += ", "
		end if
	end if

	var param = symbGetProcLastParam( proc )
	while( param )
		if( symbGetParamMode( param ) = FB_PARAMMODE_VARARG ) then
			ln += "..."
		else
			symbGetRealParamDtype( param, dtype, subtype )
			ln+=typedumpToStr( dtype, subtype )
			''with naked no parameter name
			if symbIsNaked(proc)=false then
				if symbGetParamVar( param )<>0 then ln+=" "+*symbGetMangledName(symbGetParamVar( param ))
			end if
		end if

		param = symbGetProcPrevParam( proc, param )
		if( param ) then
			ln += ", "
		end if

	wend

	ln += " )"
	ln+=" "+symbdumpToStr(proc)

	if( symbIsExport( proc ) ) then
		ln += " / dllexport"
	elseif( symbIsPrivate( proc ) ) then
		ln += " / private"
	end if


	if symbGetIsFuncPtr( proc ) then ln+=" / Accessed by funcptr"
	if (Not symbGetIsAccessed( proc )) then ln+=" / Not accessed --> not used ??"
	if  symbIsNaked(proc) then ln+=" / Naked proc"

	function = ln
end function
#endif

