'' intermediate representation - core module
''
'' chng: dec/2006 written [v1ctor]

#include once "fb.bi"
#include once "fbint.bi"
#include once "ir.bi"
#include once "emit.bi"
#include once "ir-private.bi"

dim shared ir as IRCTX

sub irInit( )
	select case( env.clopt.backend )
	case FB_BACKEND_GCC
		ir.vtbl = irhlc_vtbl
	case FB_BACKEND_LLVM
		ir.vtbl = irllvm_vtbl
	case FB_BACKEND_GAS64
		ir.vtbl = irgas64_vtbl
	case else
		assert( env.clopt.backend = FB_BACKEND_GAS )
		ir.vtbl = irtac_vtbl
	end select
	ir.vtbl.init( )
end sub

sub irEnd( )
	ir.vtbl.end( )
end sub

dim shared irhl as IRHLCONTEXT

sub irhlInit( )
	flistInit( @irhl.vregs, IR_INITVREGNODES, sizeof( IRVREG ) )
	listInit( @irhl.callargs, 32, sizeof( IRCALLARG ), LIST_FLAGS_NOCLEAR )
end sub

sub irhlEnd( )
	listEnd( @irhl.callargs )
	flistEnd( @irhl.vregs )
end sub

sub irhlEmitProcBegin( )
	irhl.regcount = 0
end sub

sub irhlEmitProcEnd( )
	flistReset( @irhl.vregs )
end sub

sub irhlEmitPushArg _
	( _
		byval param as FBSYMBOL ptr, _
		byval vr as IRVREG ptr, _
		byval udtlen as longint, _
		byval level as integer _
	)

	'' Remember for later, so during _emitCall[Ptr] we can emit the whole
	'' call in one go
	dim as IRCALLARG ptr arg = listNewNode( @irhl.callargs )
	arg->param = param
	arg->vr = vr
	arg->level = level

end sub

function irhlNewVreg _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval vtype as integer _
	) as IRVREG ptr

	dim as IRVREG ptr v = any

	v = flistNewItem( @irhl.vregs )

	v->typ = vtype
	v->dtype = dtype
	v->subtype = subtype
	if( vtype = IR_VREGTYPE_REG ) then
		v->reg = irhl.regcount
		irhl.regcount += 1
	else
		v->reg = INVALID
	end if
	v->regFamily = 0
	v->vector = 0
	v->sym = NULL
	v->ofs = 0
	v->mult = 0
	v->vidx = NULL
	v->vaux = NULL

	function = v
end function

function irhlAllocVreg _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr _
	) as IRVREG ptr
	function = irhlNewVreg( dtype, subtype, IR_VREGTYPE_REG )
end function

function irhlAllocVrImm _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval value as longint _
	) as IRVREG ptr

	dim as IRVREG ptr vr = any

	vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_IMM )
	vr->value.i = value

	function = vr
end function

function irhlAllocVrImmF _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval value as double _
	) as IRVREG ptr

	dim as IRVREG ptr vr = any

	vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_IMM )
	vr->value.f = value

	function = vr
end function

function irhlAllocVrVar _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval symbol as FBSYMBOL ptr, _
		byval ofs as longint _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_VAR )

	vr->sym = symbol
	vr->ofs = ofs

	function = vr
end function

function irhlAllocVrIdx _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval symbol as FBSYMBOL ptr, _
		byval ofs as longint, _
		byval mult as integer, _
		byval vidx as IRVREG ptr _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_IDX )

	vr->sym = symbol
	vr->ofs = ofs
	vr->vidx = vidx

	function = vr
end function

function irhlAllocVrPtr _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval ofs as longint, _
		byval vidx as IRVREG ptr _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_PTR )

	vr->ofs = ofs
	vr->vidx = vidx

	function = vr
end function

function irhlAllocVrOfs _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval symbol as FBSYMBOL ptr, _
		byval ofs as longint _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_OFS )

	vr->sym = symbol
	vr->ofs = ofs

	function = vr
end function

'' DATA descriptor arrays must be emitted based on the order indicated by the
'' FBSYMBOL.var_.data.prev linked list, not in the symtb order.
sub irForEachDataStmt( byval callback as sub( byval as FBSYMBOL ptr ) )
	var sym = astGetLastDataStmtSymbol( )
	while( sym )
		callback( sym )
		sym = sym->var_.data.prev
	wend
end sub

sub irhlFlushStaticInitializer( byval sym as FBSYMBOL ptr )
	astLoadStaticInitializer( symbGetTypeIniTree( sym ), sym )
	symbSetTypeIniTree( sym, NULL )
end sub

