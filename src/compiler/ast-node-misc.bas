'' AST misc nodes
''
'' chng: sep/2004 written [v1ctor]


#include once "fb.bi"
#include once "fbint.bi"
#include once "lex.bi"
#include once "parser.bi"
#include once "ir.bi"
#include once "ast.bi"
#include once "emit.bi"

'' Labels (l = NULL; r = NULL)
function astNewLABEL _
	( _
		byval sym as FBSYMBOL ptr, _
		byval doflush as integer _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = astNewNode( AST_NODECLASS_LABEL, FB_DATATYPE_INVALID )

	n->sym = sym
	n->lbl.flush = doflush

	if( symbIsLabel( sym ) ) then
		if( symbGetLabelIsDeclared( sym ) = FALSE ) then
			symbSetLabelIsDeclared( sym )
			symbGetLabelStmt( sym ) = parser.stmt.cnt
			symbGetLabelParent( sym ) = parser.currblock
		end if
	end if

	function = n
end function

function astLoadLABEL( byval n as ASTNODE ptr ) as IRVREG ptr
	if( ast.doemit ) then
		if( n->lbl.flush ) then
			irEmitLABEL( n->sym )
		else
			irEmitLABELNF( n->sym )
		end if
	end if

	function = NULL
end function

'' Literals (l = NULL; r = NULL)
function astNewLIT( byval text as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = astNewNode( AST_NODECLASS_LIT, FB_DATATYPE_INVALID )

	n->lit.text = ZstrAllocate( len( *text ) )
	*n->lit.text = *text

	function = n
end function

function astLoadLIT( byval n as ASTNODE ptr ) as IRVREG ptr
	if( ast.doemit ) then
		irEmitCOMMENT( n->lit.text )
	end if

	ZstrFree( n->lit.text )

	function = NULL
end function

private function astAsmAppend _
	( _
		byval tail as ASTASMTOK ptr, _
		byval typ as integer _
	) as ASTASMTOK ptr

	dim as ASTASMTOK ptr asmtok = any

	asmtok = listNewNode( @ast.asmtoklist )

	if( tail ) then
		tail->next = asmtok
	end if
	asmtok->type = typ
	asmtok->next = NULL

	function = asmtok
end function

function astAsmAppendText _
	( _
		byval tail as ASTASMTOK ptr, _
		byval text as zstring ptr _
	) as ASTASMTOK ptr

	tail = astAsmAppend( tail, AST_ASMTOK_TEXT )

	tail->text = ZstrAllocate( len( *text ) )
	*tail->text = *text

	function = tail
end function

function astAsmAppendSymb _
	( _
		byval tail as ASTASMTOK ptr, _
		byval sym as FBSYMBOL ptr _
	) as ASTASMTOK ptr

	tail = astAsmAppend( tail, AST_ASMTOK_SYMB )

	tail->sym = sym

	function = tail
end function

'' ASM (l = NULL; r = NULL)
function astNewASM( byval asmtokhead as ASTASMTOK ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = astNewNode( AST_NODECLASS_ASM, FB_DATATYPE_INVALID )

	n->asm.tokhead = asmtokhead

	function = n
end function

function astLoadASM( byval n as ASTNODE ptr ) as IRVREG ptr
	if( ast.doemit ) then
		irEmitAsmLine( n->asm.tokhead )
	end if

	var node = n->asm.tokhead
	while( node )
		var nxt = node->next

		select case( node->type )
		case AST_ASMTOK_TEXT
			ZstrFree( node->text )
		end select

		listDelNode( @ast.asmtoklist, node )
		node = nxt
	wend

	function = NULL
end function

'' Debug (l = NULL; r = NULL)
function astNewDBG _
	( _
		byval op as integer, _
		byval ex as integer, _
		byval filename As ZString Ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	if( env.clopt.debuginfo = FALSE ) then
		return null
	end if

	n = astNewNode( AST_NODECLASS_DBG, FB_DATATYPE_INVALID )

	n->dbg.op = op
	n->dbg.ex = ex
	n->dbg.filename = filename

	function = n
end function

function astLoadDBG( byval n as ASTNODE ptr ) as IRVREG ptr
	if( ast.doemit ) then
		irEmitDBG( n->dbg.op, astGetProc( )->sym, n->dbg.ex, n->dbg.filename )
	end if

	function = NULL
end function

'' No Operation (l = NULL; r = NULL)
function astNewNOP( ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = astNewNode( AST_NODECLASS_NOP, FB_DATATYPE_INVALID )

	function = n
end function

function astLoadNOP( byval n as ASTNODE ptr ) as IRVREG ptr
	'' do nothing
	function = NULL
end function

'' Non-Indexed Array (l = expr; r = NULL)
function astNewNIDXARRAY( byval expr as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = astNewNode( AST_NODECLASS_NIDXARRAY, FB_DATATYPE_INVALID )

	n->l = expr

	function = n
end function

function astLoadNIDXARRAY( byval n as ASTNODE ptr ) as IRVREG ptr
	astDelTree( n->l )
	function = NULL
end function

function astRemoveNIDXARRAY( byval n as ASTNODE ptr ) as ASTNODE ptr
	function = n
	if( astIsNIDXARRAY( n ) ) then
		function = n->l
		n->l = NULL
		astDelTree( n )
	end if
end function

'' Links (l = statement 1; r = statement 2)
function astNewLINK _
	( _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr, _
		byval ret_left as integer, _
		byval discard_result as integer _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	if( discard_result = TRUE ) then
		if( l <> NULL ) then
			if( astIsCALL( l ) ) then
				astSetType(l,FB_DATATYPE_VOID,NULL)
			end if
		end if
		if( (r <> NULL) and (l <> NULL) ) then
			if astIsCALL(r) then
				astSetType(r,FB_DATATYPE_VOID,NULL)
			end if
		end if
	end if

	if( l = NULL ) then
		return r
	end if

	if( r = NULL ) then
		return l
	end if
	if( ret_left ) then
		n = astNewNode( AST_NODECLASS_LINK, astGetFullType( l ), l->subtype )
	else
		n = astNewNode( AST_NODECLASS_LINK, astGetFullType( r ), r->subtype )
	end if

	n->link.ret_left = ret_left
	n->l = l
	n->r = r

	function = n
end function

function astLoadLINK( byval n as ASTNODE ptr ) as IRVREG ptr
	dim as IRVREG ptr vrl = any, vrr = any

	vrl = astLoad( n->l )
	astDelNode( n->l )

	vrr = astLoad( n->r )
	astDelNode( n->r )

	if( n->link.ret_left ) then
		function = vrl
	else
		function = vrr
	end if
end function

'' Explicit loads (l = expression to load to a register; r = NULL)
function astNewLOAD _
	( _
		byval l as ASTNODE ptr, _
		byval dtype as integer, _
		byval isresult as integer _
	) as ASTNODE ptr

	'' alloc new node
	dim as ASTNODE ptr n = astNewNode( AST_NODECLASS_LOAD, dtype )

	n->l  = l
	n->lod.isres = isresult

	function = n
end function

function astLoadLOAD( byval n as ASTNODE ptr ) as IRVREG ptr
	dim as ASTNODE ptr l = any
	dim as IRVREG ptr v1 = any, vr = any

	l = n->l
	if( l = NULL ) then
		return NULL
	end if

	v1 = astLoad( l )

	if( ast.doemit ) then
		if( n->lod.isres ) then
			vr = irAllocVREG( v1->dtype, v1->subtype )
			irEmitLOADRES( v1, vr )
		else
			irEmitLOAD( v1 )
		end if
	end if

	astDelNode( l )

	function = v1
end function

'' Field accesses - used in expression trees to be able to identify bitfield
'' assignments/accesses, and also by astOptimizeTree() to optimize nested field
'' accesses, and by hFbImageExpr() to identify array field accesses.
'' l = field access; r = NULL
function astNewFIELD _
	( _
		byval l as ASTNODE ptr, _
		byval sym as FBSYMBOL ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any
	dim as integer dtype = any
	dim as FBSYMBOL ptr subtype = any

	dtype = l->dtype
	subtype = l->subtype

	assert( symbIsField( sym ) )
	if( symbFieldIsBitfield( sym ) ) then
		if( typeGetDtAndPtrOnly( dtype ) = FB_DATATYPE_BOOLEAN ) then
			'' final type is always a signed int
			dtype = typeJoin( dtype, FB_DATATYPE_INTEGER )
		else
			'' final type is always an unsigned int
			dtype = typeJoin( dtype, FB_DATATYPE_UINT )
		end if
		subtype = NULL

		ast.bitfieldcount += 1

		'' Note: We can't generate bitfield access code here yet,
		'' because we don't know whether this will be a load from or
		'' store to a bitfield.
	end if

	'' Don't nest FIELD nodes, it's useless (though probably not harmful)
	'' FIELD(a, FIELD(b, ...)) => FIELD(a, ...)
	if( astIsFIELD( l ) ) then
		'' If solving out a bitfield FIELD we'd have to adjust the ast.bitfieldcount,
		'' but that can't happen because we can't have field accesses on bitfields,
		'' because those can only be integers, not UDTs.
		assert( symbFieldIsBitfield( l->sym ) = FALSE )
		l->sym = sym
		l->dtype = dtype
		l->subtype = subtype
		return l
	end if

	n = astNewNode( AST_NODECLASS_FIELD, dtype, subtype )
	n->sym = sym
	n->l = l

	function = n
end function

'' Decrease bitfield counter for the bitfield FIELD nodes in this tree,
'' to be used on field/parameter initializers that are never astAdd()ed,
'' but only ever cloned.
sub astForgetBitfields( byval n as ASTNODE ptr )
	if( (n = NULL) or (ast.bitfieldcount <= 0) ) then
		exit sub
	end if

	if( astIsBITFIELD( n ) ) then
		ast.bitfieldcount -= 1
	end if

	astForgetBitfields( n->l )
	astForgetBitfields( n->r )
end sub

private function hMakeUintMask overload( byval bits as uinteger ) as ASTNODE ptr
	dim mask as ulongint
	if( bits >= 64 ) then
		mask = &hFFFFFFFFFFFFFFFFull
	else
		mask = (1ull shl bits) - 1
	end if
	if( not fbIs64bit( ) ) then
		mask = culng( mask )
	end if
	return astNewCONSTi( mask, FB_DATATYPE_UINT )
end function

private function hMakeUintMask overload( byval bits as uinteger, byval bitpos as uinteger ) as ASTNODE ptr
	return astNewBOP( AST_OP_SHL, hMakeUintMask( bits ), astNewCONSTi( bitpos ) )
end function

private function astSetBitfield _
	( _
		byval bitfield as FBSYMBOL ptr, _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr _
	) as ASTNODE ptr

	''
	''    l<bitfield> = r
	'' becomes:
	''    l<int> = (l<int> and mask) or ((r and bits) shl bitpos)
	''

	if( symbGetType( bitfield ) = FB_DATATYPE_BOOLEAN ) then
		l->dtype = typeJoin( bitfield->typ, FB_DATATYPE_UINT )
		l->subtype = NULL
	else
		'' Remap type from bitfield to short/integer/etc., whichever was given
		'' on the bitfield, to do a "full" field access.
		l->dtype = bitfield->typ
		l->subtype = bitfield->subtype
	end if

	'' l is reused on the rhs and thus must be duplicated
	l = astCloneTree( l )

	'' Apply a mask to retrieve all bits but the bitfield's ones
	l = astNewBOP( AST_OP_AND, l, _
		astNewUOP( AST_OP_NOT, _
			hMakeUintMask( bitfield->var_.bits, bitfield->var_.bitpos ) ) )

	'' This ensures the bitfield is zeroed & clean before the new value
	'' is ORed in below. Since the new value may contain zeroes while the
	'' old values may have one-bits, the OR alone wouldn't necessarily
	'' overwrite the old value.

	'' boolean bitfield? - do a bool conversion before the bitfield store
	if( symbGetType( bitfield ) = FB_DATATYPE_BOOLEAN ) then
		if( (r->class <> AST_NODECLASS_CONV) orelse (astGetFullType( r ) <> FB_DATATYPE_BOOLEAN) ) then
			r = astNewCONV( FB_DATATYPE_BOOLEAN, NULL, r )
		end if
		r = astNewCONV( FB_DATATYPE_UINT, NULL, r )
		r = astNewBOP( AST_OP_AND, r, hMakeUintMask( bitfield->var_.bits, bitfield->var_.bitpos ) )
	else
		'' Truncate r if it's too big, ensuring the OR below won't touch any
		'' other bits outside the target bitfield.
		r = astNewBOP( AST_OP_AND, r, hMakeUintMask( bitfield->var_.bits ) )

		'' Move r into position if the bitfield doesn't lie at the beginning of
		'' the accessed field.
		if( bitfield->var_.bitpos > 0 ) then
			r = astNewBOP( AST_OP_SHL, r, astNewCONSTi( bitfield->var_.bitpos ) )
		end if
	end if

	'' OR in the new bitfield value r
	function = astNewBOP( AST_OP_OR, l, r )
end function

private function astAccessBitfield _
	( _
		byval bitfield as FBSYMBOL ptr, _
		byval l as ASTNODE ptr _
	) as ASTNODE ptr

	''    l<bitfield>
	'' becomes:
	''    (l<int> shr bitpos) and mask

	'' Remap type from bitfield to short/integer/etc, while keeping in
	'' mind that the bitfield may have been casted, so the FIELD's type
	'' can't just be discarded.
	'' if boolean make sure the bool conversion is after the bitfield access
	dim boolconv as integer
	if( symbGetType( bitfield ) = FB_DATATYPE_BOOLEAN ) then
		l->dtype = typeJoin( l->dtype, FB_DATATYPE_BYTE )
		l->subtype = NULL
		boolconv = TRUE
	else
		l->dtype = typeJoin( l->dtype, bitfield->typ )
		l->subtype = bitfield->subtype
		boolconv = FALSE
	end if

	'' Shift into position, other bits to the right are shifted out
	if( bitfield->var_.bitpos > 0 ) then
		l = astNewBOP( AST_OP_SHR, l, astNewCONSTi( bitfield->var_.bitpos ) )
	end if

	'' Mask out other bits to the left
	l = astNewBOP( AST_OP_AND, l, hMakeUintMask( bitfield->var_.bits ) )

	'' do boolean conversion after bitfield access
	if( boolconv ) then
		l->dtype = typeJoin( l->dtype, bitfield->typ )
		l->subtype = bitfield->subtype
		l = astNewCONV( FB_DATATYPE_INTEGER, NULL, l )
	end if

	function = l
end function


'' Remove FIELD nodes that mark bitfield accesses/assignments and add the
'' corresponding code instead. Non-bitfield FIELD nodes stay in,
'' they're used by astProcVectorize().
function astUpdateBitfields( byval n as ASTNODE ptr ) as ASTNODE ptr
	'' Shouldn't miss any bitfields
	assert( astCountBitfields( n ) <= ast.bitfieldcount )

	if( ast.bitfieldcount <= 0 ) then
		return n
	end if

	if( n = NULL ) then
		return NULL
	end if

	select case( n->class )
	case AST_NODECLASS_ASSIGN
		'' Assigning to a bitfield?
		if( n->l->class = AST_NODECLASS_FIELD ) then
			var bitfield = n->l->sym
			if( symbFieldIsBitfield( bitfield ) ) then
				'' Delete and link out the FIELD
				ast.bitfieldcount -= 1
				astDelNode( n->l )
				n->l = n->l->l

				'' The lhs' type is adjusted, and the new rhs
				'' is returned.
				n->r = astSetBitfield( bitfield, n->l, n->r )
			end if
		end if

	case AST_NODECLASS_FIELD
		if( symbFieldIsBitfield( n->sym ) ) then
			var l = n->l
			l = astAccessBitfield( n->sym, l )

			'' Delete and link out the FIELD
			ast.bitfieldcount -= 1
			astDelNode( n )
			n = l

			return astUpdateBitfields( n )
		end if

	end select

	n->l = astUpdateBitfields( n->l )
	n->r = astUpdateBitfields( n->r )

	function = n
end function

function astLoadFIELD( byval n as ASTNODE ptr ) as IRVREG ptr
	dim as IRVREG ptr vr = any

	vr = astLoad( n->l )
	astDelNode( n->l )

	if( ast.doemit ) then
		vr->vector = n->vector
	end if

	function = vr
end function

'' Stack operations (l = expression; r = NULL)

function astNewSTACK _
	( _
		byval op as integer, _
		byval l as ASTNODE ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	if( l = NULL ) then
		return NULL
	end if

	n = astNewNode( AST_NODECLASS_STACK, astGetFullType( l ), NULL )

	n->stack.op = op
	n->l = l

	function = n
end function

function astLoadSTACK( byval n as ASTNODE ptr ) as IRVREG ptr
	dim as ASTNODE ptr l = any
	dim as IRVREG ptr vr = any

	l  = n->l
	if( l = NULL ) then
		return NULL
	end if

	vr = astLoad( l )

	if( ast.doemit ) then
		irEmitSTACK( n->stack.op, vr )
	end if

	astDelNode( l )

	function = vr
end function
