'' quirk conditional statement (IIF) parsing
''
'' chng: sep/2004 written [v1ctor]


#include once "fb.bi"
#include once "fbint.bi"
#include once "parser.bi"
#include once "ast.bi"

'':::::
''cIIFFunct =   IIF '(' condexpr ',' truexpr ',' falsexpr ')' .
''
function cIIFFunct _
	( _
		byref funcexpr as ASTNODE ptr _
	) as integer

	dim as ASTNODE ptr condexpr = any, truexpr = any, falsexpr = any

	function = FALSE

	'' IIF
	lexSkipToken( )

	'' '('
	hMatchLPRNT( )

	'' condexpr
	hMatchExpressionEx( condexpr, FB_DATATYPE_INTEGER )

	'' ','
	hMatchCOMMA( )

	'' truexpr
	hMatchExpressionEx( truexpr, FB_DATATYPE_INTEGER )

	'' ','
	hMatchCOMMA( )

	'' falsexpr
	hMatchExpressionEx( falsexpr, astGetDataType( truexpr ) )

	'' ')'
	hMatchRPRNT( )

	''
	funcexpr = astNewIIF( condexpr, truexpr, falsexpr )

	if( funcexpr = NULL ) then
		errReport( FB_ERRMSG_INVALIDDATATYPES, TRUE )
		'' error recovery: fake an expr
		funcexpr = astNewCONSTi( 0, FB_DATATYPE_INTEGER )
	end if

	function = TRUE

end function