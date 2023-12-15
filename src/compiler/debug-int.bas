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

'#if (__FB_DEBUG__ <> 0) orelse defined(__GAS64_DEBUG__)
function emitDumpRegName( byval dtype as integer, byval reg as integer ) as string
	function = *hGetRegName( dtype, reg )
end function

#endif
