class IdrisError extends Error { }

function __prim_js2idris_array(x){
  if(x.length === 0){
    return {h:0}
  } else {
    return {a1:x[0],a2: __prim_js2idris_array(x.slice(1))}
  }
}

function __prim_idris2js_array(x){
  const result = Array();
  while (x.h === undefined) {
    result.push(x.a1); x = x.a2;
  }
  return result;
}

function __lazy(thunk) {
  let res;
  return function () {
    if (thunk === undefined) return res;
    res = thunk();
    thunk = undefined;
    return res;
  };
};

function __prim_stringIteratorNew(str) {
  return 0
}

function __prim_stringIteratorToString(_, str, it, f) {
  return f(str.slice(it))
}

function __prim_stringIteratorNext(str, it) {
  if (it >= str.length)
    return {h: 0};
  else
    return {a1: str.charAt(it), a2: it + 1};
}

function __tailRec(f,ini) {
  let obj = ini;
  while(true){
    switch(obj.h){
      case 0: return obj.a1;
      default: obj = f(obj);
    }
  }
}

const _idrisworld = Symbol('idrisworld')

const _crashExp = x=>{throw new IdrisError(x)}

const _bigIntOfString = s=> {
  try {
    const idx = s.indexOf('.')
    return idx === -1 ? BigInt(s) : BigInt(s.slice(0, idx))
  } catch (e) { return 0n }
}

const _numberOfString = s=> {
  try {
    const res = Number(s);
    return isNaN(res) ? 0 : res;
  } catch (e) { return 0 }
}

const _intOfString = s=> Math.trunc(_numberOfString(s))

const _truncToChar = x=> String.fromCodePoint(
  (x >= 0 && x <= 55295) || (x >= 57344 && x <= 1114111) ? x : 0
)

// Int8
const _truncInt8 = x => {
  const res = x & 0xff;
  return res >= 0x80 ? res - 0x100 : res;
}

const _truncBigInt8 = x => {
  const res = Number(x & 0xffn);
  return res >= 0x80 ? res - 0x100 : res;
}

const _add8s = (a,b) => _truncInt8(a + b)
const _sub8s = (a,b) => _truncInt8(a - b)
const _mul8s = (a,b) => _truncInt8(a * b)
const _div8s = (a,b) => _truncInt8(Math.trunc(a / b))
const _shl8s = (a,b) => _truncInt8(a << b)
const _shr8s = (a,b) => _truncInt8(a >> b)

// Int16
const _truncInt16 = x => {
  const res = x & 0xffff;
  return res >= 0x8000 ? res - 0x10000 : res;
}

const _truncBigInt16 = x => {
  const res = Number(x & 0xffffn);
  return res >= 0x8000 ? res - 0x10000 : res;
}

const _add16s = (a,b) => _truncInt16(a + b)
const _sub16s = (a,b) => _truncInt16(a - b)
const _mul16s = (a,b) => _truncInt16(a * b)
const _div16s = (a,b) => _truncInt16(Math.trunc(a / b))
const _shl16s = (a,b) => _truncInt16(a << b)
const _shr16s = (a,b) => _truncInt16(a >> b)

//Int32
const _truncInt32 = x => x & 0xffffffff

const _truncBigInt32 = x => {
  const res = Number(x & 0xffffffffn);
  return res >= 0x80000000 ? res - 0x100000000 : res;
}

const _add32s = (a,b) => _truncInt32(a + b)
const _sub32s = (a,b) => _truncInt32(a - b)
const _div32s = (a,b) => _truncInt32(Math.trunc(a / b))

const _mul32s = (a,b) => {
  const res = a * b;
  if (res <= Number.MIN_SAFE_INTEGER || res >= Number.MAX_SAFE_INTEGER) {
    return _truncInt32((a & 0xffff) * b + (b & 0xffff) * (a & 0xffff0000))
  } else {
    return _truncInt32(res)
  }
}

//Int64
const _truncBigInt64 = x => {
  const res = x & 0xffffffffffffffffn;
  return res >= 0x8000000000000000n ? res - 0x10000000000000000n : res;
}

const _add64s = (a,b) => _truncBigInt64(a + b)
const _sub64s = (a,b) => _truncBigInt64(a - b)
const _mul64s = (a,b) => _truncBigInt64(a * b)
const _div64s = (a,b) => _truncBigInt64(a / b)
const _shl64s = (a,b) => _truncBigInt64(a << b)
const _shr64s = (a,b) => _truncBigInt64(a >> b)

//Bits8
const _truncUInt8 = x => x & 0xff

const _truncUBigInt8 = x => Number(x & 0xffn)

const _add8u = (a,b) => (a + b) & 0xff
const _sub8u = (a,b) => (a - b) & 0xff
const _mul8u = (a,b) => (a * b) & 0xff
const _div8u = (a,b) => Math.trunc(a / b)
const _shl8u = (a,b) => (a << b) & 0xff
const _shr8u = (a,b) => (a >> b) & 0xff

//Bits16
const _truncUInt16 = x => x & 0xffff

const _truncUBigInt16 = x => Number(x & 0xffffn)

const _add16u = (a,b) => (a + b) & 0xffff
const _sub16u = (a,b) => (a - b) & 0xffff
const _mul16u = (a,b) => (a * b) & 0xffff
const _div16u = (a,b) => Math.trunc(a / b)
const _shl16u = (a,b) => (a << b) & 0xffff
const _shr16u = (a,b) => (a >> b) & 0xffff

//Bits32
const _truncUBigInt32 = x => Number(x & 0xffffffffn)

const _truncUInt32 = x => {
  const res = x & -1;
  return res < 0 ? res + 0x100000000 : res;
}

const _add32u = (a,b) => _truncUInt32(a + b)
const _sub32u = (a,b) => _truncUInt32(a - b)
const _mul32u = (a,b) => _truncUInt32(_mul32s(a,b))
const _div32u = (a,b) => Math.trunc(a / b)

const _shl32u = (a,b) => _truncUInt32(a << b)
const _shr32u = (a,b) => _truncUInt32(a <= 0x7fffffff ? a >> b : (b == 0 ? a : (a >> b) ^ ((-0x80000000) >> (b-1))))
const _and32u = (a,b) => _truncUInt32(a & b)
const _or32u = (a,b)  => _truncUInt32(a | b)
const _xor32u = (a,b) => _truncUInt32(a ^ b)

//Bits64
const _truncUBigInt64 = x => x & 0xffffffffffffffffn

const _add64u = (a,b) => (a + b) & 0xffffffffffffffffn
const _mul64u = (a,b) => (a * b) & 0xffffffffffffffffn
const _div64u = (a,b) => a / b
const _shl64u = (a,b) => (a << b) & 0xffffffffffffffffn
const _shr64u = (a,b) => (a >> b) & 0xffffffffffffffffn
const _sub64u = (a,b) => (a - b) & 0xffffffffffffffffn

//String
const _strReverse = x => x.split('').reverse().join('')

const _substr = (o,l,x) => x.slice(o, o + l)

const Web_Dom_prim__document = (()=>document);
const Prelude_Types_fastUnpack = ((str)=>__prim_js2idris_array(Array.from(str)));
const Prelude_Types_fastPack = ((xs)=>''.concat(...__prim_idris2js_array(xs)));
const Prelude_Types_fastConcat = ((xs)=>''.concat(...__prim_idris2js_array(xs)));
const JS_Util_prim__typeOf = (v=>typeof(v));
const JS_Util_prim__show = (x=>String(x));
const JS_Util_prim__eqv = ((a,b)=>a === b?1:0);
const JS_Util_prim__consoleLog = (x=>console.log(x));
const JS_Inheritance_prim__hasProtoName = ((s,v)=>{
var o = v;
  while (o != null) {
    var p = Object.getPrototypeOf(o);
    var cn = p.constructor.name;
    if (cn === s) {
      return 1;
    } else if (cn === "Object") {
      return 0;
    }
    o = p;
  }
  return 0;
});
const JS_Nullable_prim__null = (()=>null);
const JS_Boolean_true = (()=>true);
const JS_Boolean_false = (()=>false);
const Web_Internal_DomPrim_Node_prim__textContent = (x=>x.textContent);
const Web_Internal_DomPrim_Node_prim__setTextContent = ((x,v)=>{x.textContent = v});
const Web_Internal_DomPrim_InnerHTML_prim__setInnerHTML = ((x,v)=>{x.innerHTML = v});
const Web_Internal_DomPrim_Element_prim__insertAdjacentHTML = ((x,a,b)=>x.insertAdjacentHTML(a,b));
const Web_Internal_DomPrim_InnerHTML_prim__innerHTML = (x=>x.innerHTML);
const Web_Internal_DomPrim_NonElementParentNode_prim__getElementById = ((x,a)=>x.getElementById(a));
const Control_Monad_Dom_Event_prim__etId = ((et)=> et.target.id === undefined ? '' : et.target.id);
const Web_Internal_UIEventsPrim_MouseEvent_prim__shiftKey = (x=>x.shiftKey);
const Web_Internal_UIEventsPrim_MouseEvent_prim__metaKey = (x=>x.metaKey);
const Web_Internal_HtmlPrim_MouseEventHandler_prim__toMouseEventHandler = (x=>(a)=>x(a)());
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOndblclick = ((x,v)=>{x.ondblclick = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnclick = ((x,v)=>{x.onclick = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__ondblclick = (x=>x.ondblclick);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onclick = (x=>x.onclick);
function x24tcOpt_5($0) {
 switch($0.a1.h) {
  case undefined: {
   switch($0.a1.a1.h) {
    case 0: {
     switch($0.a1.a1.a1) {
      case 'id': return {h: 0, a1: {a1: $0.a1.a1.a2}};
      default: return {h: 1, a1: $0.a1.a2};
     }
    }
    default: return {h: 1, a1: $0.a1.a2};
   }
  }
  case 0: return {h: 0, a1: {h: 0}};
 }
}

function Text_Html_Attribute_getId($0) {
 return __tailRec(x24tcOpt_5, {h: 1, a1: $0});
}

function x24tcOpt_4($0) {
 switch($0.a2.h) {
  case 0: return {h: 0, a1: {h: 0}};
  case undefined: {
   const $4 = $0.a1($0.a2.a1);
   switch($4.h) {
    case 0: return {h: 1, a1: $0.a1, a2: $0.a2.a2};
    case undefined: return {h: 0, a1: {a1: $4.a1, a2: Data_List_mapMaybe($0.a1, $0.a2.a2)}};
   }
  }
 }
}

function Data_List_mapMaybe($0, $1) {
 return __tailRec(x24tcOpt_4, {h: 1, a1: $0, a2: $1});
}

function x24tcOpt_3($0) {
 switch($0.a2.h) {
  case 0: return {h: 0, a1: $0.a1};
  case undefined: return {h: 1, a1: {a1: $0.a2.a1, a2: $0.a1}, a2: $0.a2.a2};
 }
}

function Prelude_Types_List_reverseOnto($0, $1) {
 return __tailRec(x24tcOpt_3, {h: 1, a1: $0, a2: $1});
}

function x24tcOpt_2($0) {
 switch($0.a3.h) {
  case 0: return {h: 0, a1: $0.a2};
  case undefined: return {h: 1, a1: $0.a1, a2: $0.a1($0.a2)($0.a3.a1), a3: $0.a3.a2};
 }
}

function Prelude_Types_foldl_Foldable_List($0, $1, $2) {
 return __tailRec(x24tcOpt_2, {h: 1, a1: $0, a2: $1, a3: $2});
}

function x24tcOpt_1($0) {
 switch($0.a8.h) {
  case undefined: return {h: 1, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: {a1: Text_Html_Node_render($0.a8.a1), a2: $0.a7}, a8: $0.a8.a2};
  case 0: return {h: 0, a1: Prelude_Types_fastConcat(Prelude_Types_List_reverse($0.a7))};
 }
}

function Text_Html_Node_n__10931_3188_go($0, $1, $2, $3, $4, $5, $6, $7) {
 return __tailRec(x24tcOpt_1, {h: 1, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7});
}

const __mainExpression_0 = __lazy(function () {
 return PrimIO_unsafePerformIO($2 => Examples_Main_main($2));
});

const csegen_2 = __lazy(function () {
 return b => a => func => $0 => $1 => Prelude_IO_map_Functor_IO(func, $0, $1);
});

const csegen_6 = __lazy(function () {
 const $5 = b => a => $6 => $7 => $8 => {
  const $9 = $6($8);
  const $c = $7($8);
  return $9($c);
 };
 return {a1: csegen_2(), a2: a => $3 => $4 => $3, a3: $5};
});

const csegen_9 = __lazy(function () {
 return b => a => $0 => $1 => $2 => {
  const $3 = $0($2);
  return $1($3)($2);
 };
});

const csegen_12 = __lazy(function () {
 const $4 = a => $5 => $6 => {
  const $7 = $5($6);
  return $7($6);
 };
 return {a1: csegen_6(), a2: csegen_9(), a3: $4};
});

const csegen_15 = __lazy(function () {
 return b => a => func => $0 => Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29(csegen_2(), func, $0);
});

const csegen_21 = __lazy(function () {
 return {a1: csegen_15(), a2: a => $3 => Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), $3), a3: b => a => $9 => $a => Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), $9, $a)};
});

const csegen_24 = __lazy(function () {
 const $4 = a => $5 => $6 => {
  const $7 = $5($6);
  return $7($6);
 };
 return {a1: csegen_6(), a2: csegen_9(), a3: $4};
});

const csegen_30 = __lazy(function () {
 return {a1: csegen_21(), a2: b => a => $3 => $4 => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_24(), $3, $4), a3: a => $b => Control_Monad_Error_Either_join_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_24(), $b)};
});

const csegen_31 = __lazy(function () {
 return {a1: csegen_24(), a2: a => $3 => $3};
});

const csegen_34 = __lazy(function () {
 return {a1: csegen_30(), a2: a => $3 => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $3)};
});

const csegen_37 = __lazy(function () {
 return Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), 0);
});

const csegen_43 = __lazy(function () {
 return {a1: csegen_21(), a2: b => a => $3 => $4 => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), $3, $4), a3: a => $b => Control_Monad_Error_Either_join_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), $b)};
});

const csegen_52 = __lazy(function () {
 return {a1: b => a => func => $1 => $2 => Control_Monad_Dom_DomIO_map_Functor_x28DomIOx20x24iox29(csegen_15(), func, $1, $2), a2: a => $a => $b => Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29(csegen_21(), $a, $b), a3: b => a => $12 => $13 => $14 => Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28DomIOx20x24iox29(csegen_21(), $12, $13, $14)};
});

const csegen_65 = __lazy(function () {
 return {a1: csegen_34(), a2: a => $3 => $3};
});

const csegen_86 = __lazy(function () {
 return {a1: $1 => JS_Marshall_toFFI_ToFFI_String_String($1), a2: $5 => JS_Marshall_fromFFI_FromFFI_String_String($5)};
});

const csegen_107 = __lazy(function () {
 return {a1: a => ({h: 0}), a2: a => b => c => $2 => $3 => ({h: 4, a1: $3, a2: $2})};
});

const csegen_115 = __lazy(function () {
 return {a1: Examples_CSS_btn(), a2: {a1: Examples_Syntax_inc(), a2: {h: 0}}};
});

const csegen_118 = __lazy(function () {
 return {a1: 13, a2: {h: 0, a1: {h: 0, a1: 5}}};
});

const csegen_133 = __lazy(function () {
 return {a1: 18, a2: {h: 0, a1: {h: 0, a1: 5}}};
});

const csegen_135 = __lazy(function () {
 return {a1: csegen_118(), a2: {a1: {a1: 20, a2: {h: 4, a1: 10}}, a2: {h: 0}}};
});

const csegen_210 = __lazy(function () {
 return {a1: $1 => Web_Internal_HtmlTypes_toFFI_ToFFI_MouseEventHandler_MouseEventHandler($1), a2: $5 => Web_Internal_HtmlTypes_fromFFI_FromFFI_MouseEventHandler_MouseEventHandler($5)};
});

function prim__add_Integer($0, $1) {
 return ($0+$1);
}

function prim__sub_Integer($0, $1) {
 return ($0-$1);
}

function prim__mul_Integer($0, $1) {
 return ($0*$1);
}

function Examples_Main_reactimateDom($0) {
 const $9 = hRef => {
  const $12 = idRef => {
   const $15 = ev => {
    const $22 = $23 => {
     switch($23.h) {
      case undefined: return $23.a1(ev);
      case 0: return csegen_37();
     }
    };
    return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $1e => (hRef.value)), $22);
   };
   const $13 = {a1: idRef, a2: $15};
   return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), $0($13), $2f => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Data_IORef_newIORef(csegen_34(), $2f), sfRef => Prelude_Interfaces_x3ex3e(csegen_43(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $41 => (hRef.value={a1: ev => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $4e => (sfRef.value)), sf1 => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Data_MSF_step({a1: csegen_52(), a2: b => a => $5d => $5e => $5f => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29(csegen_43(), $5d, $5e, $5f), a3: a => $67 => $68 => Control_Monad_Dom_DomIO_join_Monad_x28DomIOx20x24iox29(csegen_43(), $67, $68)}, ev, sf1)($13), $72 => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $78 => (sfRef.value=$72.a2))))})), () => csegen_37())));
  };
  return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Data_IORef_newIORef(csegen_34(), 0n), $12);
 };
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Data_IORef_newIORef(csegen_34(), {h: 0}), $9);
}

function Examples_Main_main($0) {
 return JS_Util_runJS(Examples_Main_reactimateDom(Examples_Syntax_ui({a1: {a1: csegen_52(), a2: b => a => $b => $c => $d => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29(csegen_30(), $b, $c, $d), a3: a => $15 => $16 => Control_Monad_Dom_DomIO_join_Monad_x28DomIOx20x24iox29(csegen_30(), $15, $16)}, a2: $1d => Control_Monad_Dom_DomIO_unique_MonadDom_x28DomIOx20x24iox29(csegen_65(), $1d), a3: es => t => $23 => $24 => $25 => $26 => Control_Monad_Dom_DomIO_insertAdjacent_MonadDom_x28DomIOx20x24iox29(csegen_65(), $23, $24, $25, $26), a4: es => t => $2f => $30 => $31 => Control_Monad_Dom_DomIO_innerHtml_MonadDom_x28DomIOx20x24iox29(csegen_65(), $2f, $30, $31), a5: es => t => $39 => $3a => $3b => Control_Monad_Dom_DomIO_text_MonadDom_x28DomIOx20x24iox29(csegen_65(), $39, $3a, $3b), a6: es => t => $43 => $44 => Control_Monad_Dom_DomIO_listenTo_MonadDom_x28DomIOx20x24iox29(csegen_65(), $43, $44)})), $0);
}

function Text_Html_Node_n__10838_3080_esc($0) {
 switch($0) {
  case '<': return '&lt;';
  case '>': return '&gt;';
  case '&': return '&amp;';
  case '\"': return '&quot;';
  case '\'': return '&#x27';
  case '\n': return '\n';
  case '\r': return '\r';
  case '\u{9}': return '\u{9}';
  default: {
   switch(Prelude_EqOrd_x3c_Ord_Char($0, ' ')) {
    case 1: return '';
    case 0: return Data_String_singleton($0);
   }
  }
 }
}

function Text_Html_Node_render($0) {
 switch($0.h) {
  case 1: return $0.a1;
  case 2: return Text_Html_Node_escape($0.a1);
  case 0: return Prelude_Types_String_x2bx2b('<', Prelude_Types_String_x2bx2b($0.a1, Prelude_Types_String_x2bx2b(Text_Html_Node_attrs($0.a5), Prelude_Types_String_x2bx2b('>', Prelude_Types_String_x2bx2b(Text_Html_Node_n__10931_3188_go($0.a1, $0.a2, $0.a3, $0.a4, $0.a6, $0.a5, {h: 0}, $0.a6), Prelude_Types_String_x2bx2b('</', Prelude_Types_String_x2bx2b($0.a1, '>')))))));
 }
}

function Text_Html_Node_escape($0) {
 return Prelude_Types_fastConcat(Prelude_Types_map_Functor_List($5 => Text_Html_Node_n__10838_3080_esc($5), Prelude_Types_fastUnpack($0)));
}

function Text_Html_Node_attrs($0) {
 const $1 = Text_Html_Attribute_displayAttributes($0);
 switch(Data_String_null($1)) {
  case 1: return '';
  case 0: return Prelude_Types_String_x2bx2b(' ', $1);
 }
}

function Web_Dom_getElementById($0) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Web_Dom_document(), $7 => Web_Raw_Dom_NonElementParentNode_getElementById($7, $0));
}

const Web_Dom_document = __lazy(function () {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $4 => Web_Dom_prim__document($4));
});

function Web_Dom_castElementById_($0, $1) {
 return Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29($4 => $5 => $6 => $7 => $8 => Prelude_IO_map_Functor_IO($6, $7, $8), $e => Prelude_Types_x3ex3ex3d_Monad_Maybe($e, $12 => $0(undefined)($12)), Web_Dom_getElementById($1));
}

function Web_Raw_Dom_Node_textContent($0) {
 return JS_Attribute_fromNullablePrim(csegen_86(), 'Node.gettextContent', $6 => $7 => Web_Internal_DomPrim_Node_prim__textContent($6, $7), $c => $d => $e => Web_Internal_DomPrim_Node_prim__setTextContent($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Dom_Element_insertAdjacentHTML($0, $1, $2) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $7 => Web_Internal_DomPrim_Element_prim__insertAdjacentHTML(Builtin_believe_me($0), $1, $2, $7));
}

function Web_Raw_Dom_InnerHTML_innerHTML($0) {
 return JS_Attribute_fromPrim(csegen_86(), 'InnerHTML.getinnerHTML', $6 => $7 => Web_Internal_DomPrim_InnerHTML_prim__innerHTML($6, $7), $c => $d => $e => Web_Internal_DomPrim_InnerHTML_prim__setInnerHTML($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Dom_NonElementParentNode_getElementById($0, $1) {
 return JS_Marshall_tryJS($4 => JS_Nullable_fromFFI_FromFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29($7 => Web_Internal_DomTypes_fromFFI_FromFFI_Element_Element($7), $4), 'NonElementParentNode.getElementById', $d => Web_Internal_DomPrim_NonElementParentNode_prim__getElementById(Builtin_believe_me($0), $1, $d));
}

function Prelude_Basics_uncurry($0, $1) {
 return $0($1.a1)($1.a2);
}

function Prelude_Basics_flip($0, $1, $2) {
 return $0($2)($1);
}

function Prelude_Basics_apply($0, $1) {
 return $0($1);
}

function Builtin_snd($0) {
 return $0.a2;
}

function Builtin_fst($0) {
 return $0.a1;
}

function Builtin_believe_me($0) {
 return $0;
}

function Prelude_Types_toList_Foldable_List($0) {
 return $0;
}

function Prelude_Types_null_Foldable_List($0) {
 switch($0.h) {
  case 0: return () => 1;
  case undefined: return () => 0;
 }
}

function Prelude_Types_map_Functor_Maybe($0, $1) {
 switch($1.h) {
  case undefined: return {a1: $0($1.a1)};
  case 0: return {h: 0};
 }
}

function Prelude_Types_map_Functor_List($0, $1) {
 switch($1.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0($1.a1), a2: Prelude_Types_map_Functor_List($0, $1.a2)};
 }
}

function Prelude_Types_foldr_Foldable_List($0, $1, $2) {
 switch($2.h) {
  case 0: return $1;
  case undefined: return $0($2.a1)(Prelude_Types_foldr_Foldable_List($0, $1, $2.a2));
 }
}

function Prelude_Types_foldlM_Foldable_List($0, $1, $2, $3) {
 return Prelude_Types_foldl_Foldable_List(ma => b => $0.a2(undefined)(undefined)(ma)($f => Prelude_Basics_flip($1, b, $f)), $0.a1.a2(undefined)($2), $3);
}

function Prelude_Types_foldMap_Foldable_List($0, $1, $2) {
 return Prelude_Types_foldl_Foldable_List(acc => elem => $0.a1(acc)($1(elem)), $0.a2, $2);
}

function Prelude_Types_compare_Ord_Nat($0, $1) {
 return Prelude_EqOrd_compareInteger($0, $1);
}

function Prelude_Types_x3ex3ex3d_Monad_Maybe($0, $1) {
 switch($0.h) {
  case 0: return {h: 0};
  case undefined: return $1($0.a1);
 }
}

function Prelude_Types_x2b_Num_Nat($0, $1) {
 return ($0+$1);
}

function Prelude_Types_List_tailRecAppend($0, $1) {
 return Prelude_Types_List_reverseOnto($1, Prelude_Types_List_reverse($0));
}

function Prelude_Types_strCons($0, $1) {
 return ($0+$1);
}

function Prelude_Types_List_reverse($0) {
 return Prelude_Types_List_reverseOnto({h: 0}, $0);
}

function Prelude_Types_prim__integerToNat($0) {
 let $1;
 switch(((0n<=$0)?1:0)) {
  case 0: {
   $1 = 0;
   break;
  }
  default: $1 = 1;
 }
 switch($1) {
  case 1: return Builtin_believe_me($0);
  case 0: return 0n;
 }
}

function Prelude_Types_ord($0) {
 return _truncInt32($0.codePointAt(0));
}

function Prelude_Types_maybe($0, $1, $2) {
 switch($2.h) {
  case 0: return $0();
  case undefined: return $1()($2.a1);
 }
}

function Prelude_Types_either($0, $1, $2) {
 switch($2.h) {
  case 0: return $0()($2.a1);
  case 1: return $1()($2.a1);
 }
}

function Prelude_Types_chr($0) {
 return _truncToChar($0);
}

function Prelude_Types_String_x2bx2b($0, $1) {
 return ($0+$1);
}

function Prelude_EqOrd_compare_Ord_Integer($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Integer($0, $1)) {
  case 1: return 0;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1)) {
    case 1: return 1;
    case 0: return 2;
   }
  }
 }
}

function Prelude_EqOrd_x3dx3d_Eq_String($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1) {
 switch($0) {
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 2: {
   switch($1) {
    case 2: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

function Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3dx3d_Eq_Double($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3dx3d_Eq_Char($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3c_Ord_Integer($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3c_Ord_Char($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3c_Ord_Bits8($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x2fx3d_Eq_Ordering($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

function Prelude_EqOrd_x2fx3d_Eq_Double($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Double($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

function Prelude_EqOrd_compareInteger($0, $1) {
 return Prelude_EqOrd_compare_Ord_Integer($0, $1);
}

function Prelude_Interfaces_traverse_($0, $1, $2) {
 const $3 = Builtin_fst($0);
 const $19 = Builtin_snd($0);
 const $18 = $19.a2(undefined)(0);
 const $6 = $3.a1(undefined)(undefined)($e => $f => Prelude_Interfaces_x2ax3e(Builtin_snd($0), $1($e), $f))($18);
 return $6($2);
}

function Prelude_Interfaces_x3ex3e($0, $1, $2) {
 return $0.a2(undefined)(undefined)($1)($c => $2());
}

function Prelude_Interfaces_x3cx24x3e($0, $1, $2) {
 return $0(undefined)(undefined)($1)($2);
}

function Prelude_Interfaces_x2ax3e($0, $1, $2) {
 return $0.a3(undefined)(undefined)($0.a1(undefined)(undefined)($13 => $14 => $14)($1))($2);
}

function PrimIO_unsafePerformIO($0) {
 return PrimIO_unsafeCreateWorld(w => PrimIO_unsafeDestroyWorld(undefined, $0(w)));
}

function PrimIO_unsafeDestroyWorld($0, $1) {
 return $1;
}

function PrimIO_unsafeCreateWorld($0) {
 return $0(_idrisworld);
}

function Prelude_Show_show_Show_Nat($0) {
 return Prelude_Show_show_Show_Integer($0);
}

function Prelude_Show_show_Show_Integer($0) {
 return Prelude_Show_showPrec_Show_Integer({h: 0}, $0);
}

function Prelude_Show_show_Show_Int32($0) {
 return Prelude_Show_showPrec_Show_Int32({h: 0}, $0);
}

function Prelude_Show_show_Show_Double($0) {
 return Prelude_Show_showPrec_Show_Double({h: 0}, $0);
}

function Prelude_Show_show_Show_Bits16($0) {
 return Prelude_Show_showPrec_Show_Bits16({h: 0}, $0);
}

function Prelude_Show_showPrec_Show_Integer($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Int32($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Double($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Bits16($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_compare_Ord_Prec($0, $1) {
 switch($0.h) {
  case 4: {
   switch($1.h) {
    case 4: return Prelude_Types_compare_Ord_Nat($0.a1, $1.a1);
    default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
   }
  }
  default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
 }
}

function Prelude_Show_x3ex3d_Ord_Prec($0, $1) {
 return Prelude_EqOrd_x2fx3d_Eq_Ordering(Prelude_Show_compare_Ord_Prec($0, $1), 0);
}

function Prelude_Show_showParens($0, $1) {
 switch($0) {
  case 0: return $1;
  case 1: return Prelude_Types_String_x2bx2b('(', Prelude_Types_String_x2bx2b($1, ')'));
 }
}

function Prelude_Show_primNumShow($0, $1, $2) {
 const $3 = $0($2);
 let $7;
 switch(Prelude_Show_x3ex3d_Ord_Prec($1, {h: 5})) {
  case 1: {
   $7 = Prelude_Show_firstCharIs($e => Prelude_EqOrd_x3dx3d_Eq_Char($e, '-'), $3);
   break;
  }
  case 0: {
   $7 = 0;
   break;
  }
 }
 return Prelude_Show_showParens($7, $3);
}

function Prelude_Show_precCon($0) {
 switch($0.h) {
  case 0: return 0n;
  case 1: return 1n;
  case 2: return 2n;
  case 3: return 3n;
  case 4: return 4n;
  case 5: return 5n;
  case 6: return 6n;
 }
}

function Prelude_Show_firstCharIs($0, $1) {
 switch($1) {
  case '': return 0;
  default: return $0(($1.charAt(0)));
 }
}

function Prelude_IO_map_Functor_IO($0, $1, $2) {
 const $3 = $1($2);
 return $0($3);
}

function Prelude_Cast_cast_Cast_Nat_Bits8($0) {
 return Prelude_Cast_cast_Cast_Integer_Bits8($0);
}

function Prelude_Cast_cast_Cast_Integer_Bits8($0) {
 return Number(_truncUBigInt8($0));
}

function Prelude_Cast_cast_Cast_Bits8_Int($0) {
 return $0;
}

function JS_Util_typeof($0) {
 return JS_Util_prim__typeOf(Builtin_believe_me($0));
}

function JS_Util_runJSWith($0, $1, $2) {
 const $3 = $1($2);
 return Prelude_Types_either(() => $0(), () => $b => $c => $b, $3)($2);
}

function JS_Util_runJS($0, $1) {
 return JS_Util_runJSWith(() => $4 => JS_Util_consoleLog(csegen_31(), JS_Util_dispErr($4)), $0, $1);
}

function JS_Util_jsShow($0) {
 return JS_Util_prim__show(Builtin_believe_me($0));
}

function JS_Util_eqv($0, $1) {
 return JS_Util_doubleToBool(JS_Util_prim__eqv(Builtin_believe_me($0), Builtin_believe_me($1)));
}

function JS_Util_doubleToBool($0) {
 return Prelude_EqOrd_x2fx3d_Eq_Double($0, 0.0);
}

function JS_Util_dispErr($0) {
 switch($0.h) {
  case 1: return Prelude_Types_String_x2bx2b('Error when casting a Javascript value in function ', Prelude_Types_String_x2bx2b($0.a1, Prelude_Types_String_x2bx2b('.\n  The value was: ', Prelude_Types_String_x2bx2b(JS_Util_jsShow($0.a2), Prelude_Types_String_x2bx2b('.\n  The value\'s type was ', Prelude_Types_String_x2bx2b(JS_Util_typeof($0.a2), '.'))))));
  case 2: return Prelude_Types_String_x2bx2b('Trying to extract a value from Nothing at ', $0.a1);
  case 0: return $0.a1;
 }
}

function JS_Util_consoleLog($0, $1) {
 return $0.a2(undefined)($7 => JS_Util_prim__consoleLog($1, $7));
}

function Control_Monad_Error_Interface_throwError_MonadError_x24e_x28x28EitherTx20x24ex29x20x24mx29($0, $1) {
 return $0.a1.a2(undefined)({h: 0, a1: $1});
}

function Data_Bits_fromNat($0, $1) {
 return $0;
}

function Data_List_mergeReplicate($0, $1) {
 switch($1.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0, a2: {a1: $1.a1, a2: Data_List_mergeReplicate($0, $1.a2)}};
 }
}

function Data_List_intersperse($0, $1) {
 switch($1.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $1.a1, a2: Data_List_mergeReplicate($0, $1.a2)};
 }
}

function Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29($0, $1) {
 return $0.a2(undefined)({h: 1, a1: $1});
}

function Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29($0, $1, $2) {
 const $5 = $6 => {
  switch($6.h) {
   case 0: return {h: 0, a1: $6.a1};
   case 1: return {h: 1, a1: $1($6.a1)};
  }
 };
 return Prelude_Interfaces_x3cx24x3e($0, $5, $2);
}

function Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29($0, $1) {
 const $6 = $7 => {
  const $8 = $1($7);
  return {h: 1, a1: $8};
 };
 return $0.a2(undefined)($6);
}

function Control_Monad_Error_Either_join_Monad_x28x28EitherTx20x24ex29x20x24mx29($0, $1) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29($0, $1, $6 => $6);
}

function Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29($0, $1, $2) {
 return $0.a2(undefined)(undefined)($1)($c => Prelude_Types_either(() => $f => $0.a1.a2(undefined)({h: 0, a1: $f}), () => $18 => $2($18), $c));
}

function Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29($0, $1, $2) {
 const $17 = $18 => $19 => {
  switch($18.h) {
   case 0: return {h: 0, a1: $18.a1};
   case 1: {
    switch($19.h) {
     case 1: return {h: 1, a1: $18.a1($19.a1)};
     case 0: return {h: 0, a1: $19.a1};
    }
   }
  }
 };
 const $12 = $0.a2(undefined)($17);
 const $c = $0.a3(undefined)(undefined)($12);
 const $a = $c($1);
 const $4 = $0.a3(undefined)(undefined)($a);
 return $4($2);
}

function JS_Marshall_toFFI_ToFFI_String_String($0) {
 return $0;
}

function JS_Marshall_fromFFI_FromFFI_String_String($0) {
 return {a1: $0};
}

function JS_Marshall_tryJS($0, $1, $2) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $2), $c => JS_Marshall_tryFromFFI($0, $1, $c));
}

function JS_Marshall_tryFromFFI($0, $1, $2) {
 const $3 = $0($2);
 switch($3.h) {
  case 0: return Control_Monad_Error_Interface_throwError_MonadError_x24e_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), {h: 1, a1: $1, a2: $2});
  case undefined: return Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), $3.a1);
 }
}

function JS_Inheritance_unsafeCastOnPrototypeName($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Double(JS_Inheritance_prim__hasProtoName($0, Builtin_believe_me($1)), 1.0)) {
  case 1: return {a1: Builtin_believe_me($1)};
  case 0: return {h: 0};
 }
}

function Data_String_n__2942_2537_unlinesx27($0) {
 switch($0.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0.a1, a2: {a1: '\n', a2: Data_String_n__2942_2537_unlinesx27($0.a2)}};
 }
}

function Data_String_singleton($0) {
 return Prelude_Types_strCons($0, '');
}

function Data_String_null($0) {
 return Prelude_EqOrd_x3dx3d_Eq_String($0, '');
}

function Data_String_fastUnlines($0) {
 return Prelude_Types_fastConcat(Data_String_n__2942_2537_unlinesx27($0));
}

function Data_SOP_NP_append($0, $1) {
 switch($0.h) {
  case 0: return $1;
  case undefined: return {a1: $0.a1, a2: Data_SOP_NP_append($0.a2, $1)};
 }
}

function JS_Nullable_toFFI_ToFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29($0, $1) {
 return JS_Nullable_maybeToNullable(Prelude_Types_map_Functor_Maybe($6 => $0($6), $1));
}

function JS_Nullable_fromFFI_FromFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29($0, $1) {
 const $2 = JS_Nullable_nullableToMaybe($1);
 switch($2.h) {
  case 0: return {a1: {h: 0}};
  case undefined: return Prelude_Types_map_Functor_Maybe($8 => ({a1: $8}), $0($2.a1));
 }
}

function JS_Nullable_nullableToMaybe($0) {
 switch(JS_Nullable_isNull($0)) {
  case 1: return {h: 0};
  case 0: return {a1: Builtin_believe_me($0)};
 }
}

const JS_Nullable_null = __lazy(function () {
 return Builtin_believe_me(JS_Nullable_prim__null());
});

function JS_Nullable_nonNull($0) {
 return Builtin_believe_me($0);
}

function JS_Nullable_maybeToNullable($0) {
 return Prelude_Types_maybe(() => JS_Nullable_null(), () => $5 => JS_Nullable_nonNull($5), $0);
}

function JS_Nullable_isNull($0) {
 return JS_Util_eqv(JS_Nullable_prim__null(), $0);
}

function JS_Boolean_fromFFI_FromFFI_Bool_Boolean($0) {
 switch(JS_Util_eqv($0, JS_Boolean_true())) {
  case 1: return {a1: 1};
  case 0: {
   switch(JS_Util_eqv($0, JS_Boolean_false())) {
    case 1: return {a1: 0};
    case 0: return {h: 0};
   }
  }
 }
}

function JS_Attribute_set($0, $1) {
 switch($0.h) {
  case 0: return $0.a2($1);
  case 1: return $0.a2({a1: $1});
  case 2: return $0.a2({a1: $1});
  case 3: return $0.a2({a1: $1});
 }
}

function JS_Attribute_fromPrim($0, $1, $2, $3, $4) {
 return {h: 0, a1: JS_Marshall_tryJS(Builtin_snd($0), $1, $2($4)), a2: a => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $3($4)(Builtin_fst($0)(a)))};
}

function JS_Attribute_fromNullablePrim($0, $1, $2, $3, $4) {
 return {h: 1, a1: JS_Marshall_tryJS($8 => JS_Nullable_fromFFI_FromFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29(Builtin_snd($0), $8), $1, $2($4)), a2: a => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $3($4)(JS_Nullable_toFFI_ToFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29(Builtin_fst($0), a)))};
}

function JS_Attribute_x21x3e($0, $1, $2) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), $0($2), $a => JS_Attribute_set($1, $a));
}

function Web_Internal_HtmlTypes_toFFI_ToFFI_MouseEventHandler_MouseEventHandler($0) {
 return $0;
}

function Web_Internal_HtmlTypes_safeCast_SafeCast_HTMLElement($0) {
 return JS_Inheritance_unsafeCastOnPrototypeName('HTMLElement', $0);
}

function Web_Internal_HtmlTypes_fromFFI_FromFFI_MouseEventHandler_MouseEventHandler($0) {
 return {a1: $0};
}

function Web_Internal_DomTypes_fromFFI_FromFFI_Element_Element($0) {
 return {a1: $0};
}

function Text_Html_Attribute_strMod($0) {
 return Text_Html_Attribute_mkMod($0, $4 => $5 => ({h: 0, a1: $4, a2: $5}));
}

function Text_Html_Attribute_mkMod($0, $1) {
 return $1($0);
}

const Text_Html_Attribute_id = __lazy(function () {
 return Text_Html_Attribute_strMod('id');
});

function Text_Html_Attribute_displayAttributes($0) {
 return Prelude_Types_fastConcat(Data_List_intersperse(' ', Data_List_mapMaybe($8 => Text_Html_Attribute_displayAttribute($8), $0)));
}

function Text_Html_Attribute_displayAttribute($0) {
 switch($0.h) {
  case 0: return {a1: Prelude_Types_String_x2bx2b($0.a1, Prelude_Types_String_x2bx2b('=\"', Prelude_Types_String_x2bx2b($0.a2, '\"')))};
  case 1: {
   switch($0.a2) {
    case 1: return {a1: $0.a1};
    case 0: return {h: 0};
   }
  }
 }
}

function Text_Html_Attribute_dispMod($0, $1) {
 return Text_Html_Attribute_mkMod($0, s => $5 => ({h: 0, a1: s, a2: $1($5)}));
}

const Text_Html_Attribute_classes = __lazy(function () {
 return Text_Html_Attribute_dispMod('class', $3 => Prelude_Types_fastConcat(Data_List_intersperse(' ', $3)));
});

const Text_Html_Attribute_class = __lazy(function () {
 return Text_Html_Attribute_strMod('class');
});

function Examples_Syntax_ui($0) {
 return Prelude_Interfaces_x3ex3e($0.a1, Examples_CSS_applyCSS($0, Prelude_Types_List_tailRecAppend(Examples_CSS_coreCSS(), Examples_Syntax_css())), () => $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Util_innerHtmlAt($0, Examples_CSS_contentDiv(), Examples_Syntax_content()))($1e => $0.a1.a1.a2(undefined)(Control_Category_x3ex3ex3e({a1: a => Data_MES_id_Category_x28MESx20x24mx29(), a2: a => b => c => $2f => $30 => Data_MES_x2e_Category_x28MESx20x24mx29($2f, $30)}, Data_MES_unionL(Data_MES_on({h: 1, a1: Number(_truncBigInt32(1n))}, Examples_Syntax_click($1e.a1)), Data_MES_on({h: 1, a1: Number(_truncBigInt32(-1n))}, Examples_Syntax_click($1e.a2.a1))), {h: 4, a1: Control_Category_x3ex3ex3e(csegen_107(), Data_MSF_accumulateWith($4e => $4f => _add32s($4e, $4f), Number(_truncBigInt32(0n))), {h: 4, a1: {h: 2, a1: $57 => Prelude_Show_show_Show_Int32($57)}, a2: Examples_Syntax_text($0, $1e.a2.a2.a1)}), a2: {h: 2, a1: $60 => ({a1: $60})}}))));
}

function Examples_Syntax_text($0, $1) {
 return {h: 3, a1: $3 => $0.a5(undefined)(undefined)($1)($3)};
}

const Examples_Syntax_output = __lazy(function () {
 return 'output';
});

const Examples_Syntax_nonlist = __lazy(function () {
 return 'nonlist';
});

const Examples_Syntax_lstline = __lazy(function () {
 return 'lstline';
});

const Examples_Syntax_lstlbl = __lazy(function () {
 return 'lstlbl';
});

function Examples_Syntax_line($0, $1) {
 return {h: 0, a1: 'div', a2: 17, a3: 0, a4: {h: 0}, a5: {a1: Text_Html_Attribute_class()(Examples_Syntax_lstline()), a2: {h: 0}}, a6: {a1: {h: 0, a1: 'label', a2: 34, a3: 0, a4: {h: 0}, a5: {a1: Text_Html_Attribute_class()(Examples_Syntax_lstlbl()), a2: {h: 0}}, a6: {a1: {h: 2, a1: $0}, a2: {h: 0}}}, a2: $1}};
}

function Examples_Syntax_incbtn($0) {
 return {h: 0, a1: 'button', a2: 7, a3: 1, a4: {a1: 0, a2: {h: 0}}, a5: {a1: Text_Html_Attribute_classes()(csegen_115()), a2: {h: 0}}, a6: {a1: {h: 2, a1: $0}, a2: {h: 0}}};
}

const Examples_Syntax_inc = __lazy(function () {
 return 'inc';
});

const Examples_Syntax_css = __lazy(function () {
 return {a1: {a1: {h: 2, a1: Examples_Syntax_nonlist()}, a2: {a1: {a1: 19, a2: 0}, a2: {a1: {a1: 7, a2: 0}, a2: {a1: {a1: 8, a2: '1'}, a2: {a1: {a1: 9, a2: 2}, a2: {a1: {a1: 12, a2: 3}, a2: {a1: csegen_118(), a2: {a1: {a1: 2, a2: Text_CSS_Color_palegreen()}, a2: {h: 0}}}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_Syntax_lstline()}, a2: {a1: {a1: 7, a2: 0}, a2: {a1: csegen_118(), a2: {h: 0}}}}, a2: {a1: {a1: {h: 2, a1: Examples_Syntax_lstlbl()}, a2: {a1: csegen_118(), a2: {a1: {a1: 20, a2: {h: 4, a1: 20}}, a2: {h: 0}}}}, a2: {a1: {a1: {h: 2, a1: Examples_Syntax_output()}, a2: {a1: csegen_133(), a2: csegen_135()}}, a2: {a1: {a1: {h: 4, a1: Prelude_Types_map_Functor_List($4c => ({h: 2, a1: $4c}), csegen_115())}, a2: csegen_135()}, a2: {h: 0}}}}}};
});

const Examples_Syntax_content = __lazy(function () {
 return {h: 0, a1: 'div', a2: 17, a3: 0, a4: {h: 0}, a5: {a1: Text_Html_Attribute_class()(Examples_Syntax_nonlist()), a2: {h: 0}}, a6: {a1: Examples_Syntax_line('Increase counter:', {a1: Examples_Syntax_incbtn('+'), a2: {h: 0}}), a2: {a1: Examples_Syntax_line('Decrease counter:', {a1: Examples_Syntax_incbtn('-'), a2: {h: 0}}), a2: {a1: Examples_Syntax_line('Count:', {a1: {h: 0, a1: 'div', a2: 17, a3: 1, a4: {h: 0}, a5: {a1: Text_Html_Attribute_class()(Examples_Syntax_output()), a2: {h: 0}}, a6: {a1: {h: 2, a1: '0'}, a2: {h: 0}}}, a2: {h: 0}}), a2: {h: 0}}}}};
});

function Examples_Syntax_click($0) {
 const $4 = $5 => {
  switch($5.h) {
   case 0: {
    switch(Prelude_EqOrd_x3dx3d_Eq_String($0.a2, $5.a1.a1)) {
     case 1: return {a1: 0};
     case 0: return {h: 0};
    }
   }
   default: return {h: 0};
  }
 };
 return Data_MES_when($4);
}

function Text_CSS_Size_render($0) {
 switch($0.h) {
  case 0: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Bits16($0.a1), 'pt');
  case 1: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Bits16($0.a1), 'px');
  case 2: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Double($0.a1), 'em');
  case 3: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Double($0.a1), 'rem');
  case 4: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Bits16($0.a1), '%');
 }
}

function Text_CSS_Selector_render($0) {
 switch($0.h) {
  case 0: return '*';
  case 1: return Prelude_Types_String_x2bx2b('#', $0.a1);
  case 2: return Prelude_Types_String_x2bx2b('.', $0.a1);
  case 3: return $0.a1;
  case 4: return Prelude_Types_fastConcat(Data_List_intersperse(', ', Prelude_Types_map_Functor_List($f => Text_CSS_Selector_render($f), $0.a1)));
 }
}

function Text_CSS_Rule_render($0) {
 return Prelude_Types_String_x2bx2b(Text_CSS_Selector_render($0.a1), Prelude_Types_String_x2bx2b('{', Prelude_Types_String_x2bx2b(Prelude_Types_fastConcat(Prelude_Types_map_Functor_List($10 => Prelude_Types_String_x2bx2b(Text_CSS_Property_renderProp($10.a1, $10.a2), ';'), $0.a2)), '}')));
}

function Text_CSS_Property_renderProp($0, $1) {
 switch($0) {
  case 0: return Prelude_Types_String_x2bx2b('align-items: ', Text_CSS_Flexbox_FlexAlign_render($1));
  case 1: return Prelude_Types_String_x2bx2b('align-self: ', Text_CSS_Flexbox_FlexAlign_render($1));
  case 2: return Prelude_Types_String_x2bx2b('background-color: ', Text_CSS_Color_render($1));
  case 3: return Text_CSS_Dir_render('border-color', $15 => Text_CSS_Color_render($15), $1);
  case 4: return Text_CSS_Dir_render('border-style', $1c => Text_CSS_Property_BorderStyle_render($1c), $1);
  case 5: return Text_CSS_Dir_render('border-width', $23 => Text_CSS_Property_BorderWidth_render($23), $1);
  case 6: return Prelude_Types_String_x2bx2b('color: ', Text_CSS_Color_render($1));
  case 7: return Prelude_Types_String_x2bx2b('display: ', Text_CSS_Property_Display_render($1));
  case 8: return Prelude_Types_String_x2bx2b('flex: ', $1);
  case 9: return Prelude_Types_String_x2bx2b('flex-direction: ', Text_CSS_Flexbox_FlexDirection_render($1));
  case 10: return Prelude_Types_String_x2bx2b('font-size: ', Text_CSS_Property_FontSize_render($1));
  case 11: return Prelude_Types_String_x2bx2b('height: ', Text_CSS_Property_Width_render($1));
  case 12: return Prelude_Types_String_x2bx2b('justify-content: ', Text_CSS_Flexbox_FlexJustify_render($1));
  case 13: return Text_CSS_Dir_render('margin', $4b => Text_CSS_Size_render($4b), $1);
  case 14: return Prelude_Types_String_x2bx2b('max-height: ', Text_CSS_Property_Width_render($1));
  case 15: return Prelude_Types_String_x2bx2b('max-width: ', Text_CSS_Property_Width_render($1));
  case 16: return Prelude_Types_String_x2bx2b('min-height: ', Text_CSS_Property_Width_render($1));
  case 17: return Prelude_Types_String_x2bx2b('min-width: ', Text_CSS_Property_Width_render($1));
  case 18: return Text_CSS_Dir_render('padding', $66 => Text_CSS_Size_render($66), $1);
  case 19: return Prelude_Types_String_x2bx2b('list-style-type: ', Text_CSS_ListStyleType_render($1));
  case 20: return Prelude_Types_String_x2bx2b('width: ', Text_CSS_Property_Width_render($1));
 }
}

function Text_CSS_Property_Width_render($0) {
 return Text_CSS_Size_render($0);
}

function Text_CSS_Property_FontSize_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Size_render($0.a1);
  case 1: return 'xx-small';
  case 2: return 'x-small';
  case 3: return 'small';
  case 4: return 'medium';
  case 5: return 'large';
  case 6: return 'x-large';
  case 7: return 'xx-large';
  case 8: return 'xxx-large';
 }
}

function Text_CSS_Property_Display_render($0) {
 return 'flex';
}

function Text_CSS_Property_BorderWidth_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Size_render($0.a1);
  case 1: return 'thin';
  case 2: return 'medium';
  case 3: return 'thick';
 }
}

function Text_CSS_Property_BorderStyle_render($0) {
 switch($0) {
  case 0: return 'none';
  case 1: return 'hidden';
  case 2: return 'dotted';
  case 3: return 'dashed';
  case 4: return 'solid';
  case 5: return 'double';
  case 6: return 'groove';
  case 7: return 'ridge';
  case 8: return 'inset';
  case 9: return 'outset';
 }
}

function Text_CSS_ListStyleType_render($0) {
 switch($0) {
  case 0: return 'none';
  case 1: return 'disc';
  case 2: return 'circle';
  case 3: return 'square';
  case 4: return 'decimal';
  case 5: return 'lower-alpha';
  case 6: return 'upper-alpha';
  case 7: return 'lower-roman';
  case 8: return 'upper-roman';
 }
}

function Text_CSS_Flexbox_FlexJustify_render($0) {
 switch($0) {
  case 0: return 'center';
  case 1: return 'start';
  case 2: return 'end';
  case 3: return 'flex-start';
  case 4: return 'flex-end';
  case 5: return 'left';
  case 6: return 'right';
  case 7: return 'normal';
  case 8: return 'space-between';
  case 9: return 'space-around';
  case 10: return 'space-evenly';
  case 11: return 'stretch';
 }
}

function Text_CSS_Flexbox_FlexDirection_render($0) {
 switch($0) {
  case 0: return 'row';
  case 1: return 'row-reverse';
  case 2: return 'column';
  case 3: return 'column-reverse';
 }
}

function Text_CSS_Flexbox_FlexAlign_render($0) {
 switch($0) {
  case 0: return 'normal';
  case 1: return 'stretch';
  case 2: return 'center';
  case 3: return 'start';
  case 4: return 'end';
  case 5: return 'flex-start';
  case 6: return 'flex-end';
  case 7: return 'baseline';
  case 8: return 'first baseline';
  case 9: return 'last baseline';
 }
}

function Text_CSS_Dir_vals($0) {
 switch($0.h) {
  case 0: return {a1: $0.a1, a2: {h: 0}};
  case 1: return {a1: $0.a1, a2: {h: 0}};
  case 2: return {a1: $0.a1, a2: {h: 0}};
  case 3: return {a1: $0.a1, a2: {h: 0}};
  case 4: return {a1: $0.a1, a2: {h: 0}};
  case 5: return {a1: $0.a1, a2: {a1: $0.a2, a2: {h: 0}}};
  case 6: return {a1: $0.a1, a2: {a1: $0.a2, a2: {a1: $0.a3, a2: {h: 0}}}};
  case 7: return {a1: $0.a1, a2: {a1: $0.a2, a2: {a1: $0.a3, a2: {a1: $0.a4, a2: {h: 0}}}}};
 }
}

function Text_CSS_Dir_render($0, $1, $2) {
 const $3 = Prelude_Types_fastConcat(Data_List_intersperse(' ', Prelude_Types_map_Functor_List($1, Text_CSS_Dir_vals($2))));
 const $e = Text_CSS_Dir_prfx($2);
 return Prelude_Types_String_x2bx2b($0, Prelude_Types_String_x2bx2b($e, Prelude_Types_String_x2bx2b(': ', $3)));
}

function Text_CSS_Dir_prfx($0) {
 switch($0.h) {
  case 1: return '-left';
  case 2: return '-right';
  case 3: return '-top';
  case 4: return '-bottom';
  default: return '';
 }
}

function Text_CSS_Color_toHexChar($0) {
 switch(Prelude_EqOrd_x3c_Ord_Bits8($0, Number(_truncUBigInt8(10n)))) {
  case 1: return Prelude_Types_chr(_add32s(Prelude_Types_ord('0'), Prelude_Cast_cast_Cast_Bits8_Int($0)));
  case 0: return Prelude_Types_chr(_sub32s(_add32s(Prelude_Types_ord('a'), Prelude_Cast_cast_Cast_Bits8_Int($0)), 10));
 }
}

function Text_CSS_Color_toHex($0) {
 return Prelude_Types_fastPack({a1: Text_CSS_Color_toHexChar(_shr8u($0, Prelude_Cast_cast_Cast_Nat_Bits8(Data_Bits_fromNat(4n, 8n)))), a2: {a1: Text_CSS_Color_toHexChar(($0&15)), a2: {h: 0}}});
}

function Text_CSS_Color_render($0) {
 return Prelude_Types_String_x2bx2b('#', Prelude_Types_String_x2bx2b(Text_CSS_Color_toHex($0.a1), Prelude_Types_String_x2bx2b(Text_CSS_Color_toHex($0.a2), Text_CSS_Color_toHex($0.a3))));
}

const Text_CSS_Color_palegreen = __lazy(function () {
 return {a1: Number(_truncUBigInt8(152n)), a2: Number(_truncUBigInt8(251n)), a3: Number(_truncUBigInt8(152n))};
});

const Text_CSS_Color_black = __lazy(function () {
 return {a1: 0, a2: 0, a3: 0};
});

const Examples_CSS_coreCSS = __lazy(function () {
 return {a1: {a1: {h: 3, a1: 'body'}, a2: {a1: {a1: 2, a2: Text_CSS_Color_black()}, a2: {a1: {a1: 7, a2: 0}, a2: {a1: {a1: 9, a2: 2}, a2: {a1: {a1: 11, a2: {h: 4, a1: Number(_truncUBigInt16(100n))}}, a2: {a1: {a1: 13, a2: {h: 0, a1: {h: 0, a1: 0}}}, a2: {h: 0}}}}}}}, a2: {a1: {a1: {h: 1, a1: 'content'}, a2: {a1: {a1: 1, a2: 2}, a2: {a1: {a1: 2, a2: {a1: 16, a2: 16, a3: 16}}, a2: {a1: {a1: 7, a2: 0}, a2: {a1: {a1: 8, a2: '1'}, a2: {a1: {a1: 9, a2: 2}, a2: {a1: {a1: 12, a2: 3}, a2: {a1: {a1: 18, a2: {h: 5, a1: {h: 0, a1: 40}, a2: {h: 0, a1: 20}}}, a2: {a1: {a1: 17, a2: {h: 4, a1: 70}}, a2: {h: 0}}}}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_btn()}, a2: {a1: csegen_133(), a2: {h: 0}}}, a2: {h: 0}}}};
});

const Examples_CSS_contentDiv = __lazy(function () {
 return {a1: 17, a2: 'content', a3: {h: 0}};
});

const Examples_CSS_btn = __lazy(function () {
 return 'btn';
});

function Examples_CSS_applyCSS($0, $1) {
 return Control_Monad_Dom_Util_rawInnerHtmlAt($0, Examples_CSS_appStyle(), Data_String_fastUnlines(Prelude_Types_map_Functor_List($b => Text_CSS_Rule_render($b), $1)));
}

const Examples_CSS_appStyle = __lazy(function () {
 return {a1: 58, a2: 'appstyle', a3: {h: 0}};
});

function Control_Monad_Dom_Util_rawInnerHtmlAt($0, $1, $2) {
 return $0.a1.a1.a1(undefined)(undefined)($d => 0)($0.a4(undefined)(undefined)($1)({h: 1, a1: $2}));
}

function Control_Monad_Dom_Util_listenToNodes($0, $1) {
 switch($1.h) {
  case 0: return $0.a1.a1.a2(undefined)(0);
  case undefined: return Prelude_Interfaces_x3ex3e($0.a1, Control_Monad_Dom_Util_listenToNode($0, $1.a1), () => Control_Monad_Dom_Util_listenToNodes($0, $1.a2));
 }
}

function Control_Monad_Dom_Util_listenToNode($0, $1) {
 switch($1.h) {
  case 0: {
   const $3 = Text_Html_Attribute_getId($1.a5);
   switch($3.h) {
    case undefined: return Prelude_Interfaces_x3ex3e($0.a1, $0.a6(undefined)(undefined)({a1: $1.a2, a2: $3.a1, a3: $1.a4}), () => Control_Monad_Dom_Util_listenToNodes($0, $1.a6));
    case 0: return Control_Monad_Dom_Util_listenToNodes($0, $1.a6);
   }
  }
  case 1: return $0.a1.a1.a2(undefined)(0);
  case 2: return $0.a1.a1.a2(undefined)(0);
 }
}

function Control_Monad_Dom_Util_innerHtmlAt($0, $1, $2) {
 return $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Ref_mkNode($0, $2))($10 => Prelude_Interfaces_x3ex3e($0.a1, $0.a4(undefined)(undefined)($1)($10.a2), () => Prelude_Interfaces_x3ex3e($0.a1, Control_Monad_Dom_Util_listenToNode($0, $10.a2), () => $0.a1.a1.a2(undefined)($10.a1))));
}

function Control_Monad_Dom_Ref_mkRef($0, $1, $2) {
 const $d = n => {
  const $e = Prelude_Types_String_x2bx2b('uid', Prelude_Show_show_Show_Nat(n));
  return $0.a1.a1.a2(undefined)({a1: {a1: $1, a2: $e, a3: $2}, a2: $e});
 };
 return $0.a1.a2(undefined)(undefined)($0.a2)($d);
}

function Control_Monad_Dom_Ref_mkNodes($0, $1) {
 switch($1.h) {
  case 0: return $0.a1.a1.a2(undefined)({a1: {h: 0}, a2: {h: 0}});
  case undefined: return $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Ref_mkNode($0, $1.a1))($19 => $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Ref_mkNodes($0, $1.a2))($28 => $0.a1.a1.a2(undefined)({a1: Data_SOP_NP_append($19.a1, $28.a1), a2: {a1: $19.a2, a2: $28.a2}})));
 }
}

function Control_Monad_Dom_Ref_mkNode($0, $1) {
 switch($1.h) {
  case 0: {
   switch($1.a3) {
    case 1: {
     const $d = Text_Html_Attribute_getId($1.a5);
     let $c;
     switch($d.h) {
      case undefined: {
       $c = $0.a1.a1.a2(undefined)({a1: {a1: $1.a2, a2: $d.a1, a3: $1.a4}, a2: $1.a5});
       break;
      }
      case 0: {
       $c = $0.a1.a1.a1(undefined)(undefined)($26 => ({a1: $26.a1, a2: {a1: Text_Html_Attribute_id()($26.a2), a2: $1.a5}}))(Control_Monad_Dom_Ref_mkRef($0, $1.a2, $1.a4));
       break;
      }
     }
     const $6 = $0.a1.a2(undefined)(undefined)($c);
     return $6($35 => $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Ref_mkNodes($0, $1.a6))($44 => $0.a1.a1.a2(undefined)({a1: {a1: $35.a1, a2: $44.a1}, a2: {h: 0, a1: $1.a1, a2: $1.a2, a3: 1, a4: $1.a4, a5: $35.a2, a6: $44.a2}})));
    }
    case 0: return Prelude_Interfaces_x3cx24x3e($0.a1.a1.a1, $5d => ({a1: $5d.a1, a2: {h: 0, a1: $1.a1, a2: $1.a2, a3: 0, a4: $1.a4, a5: $1.a5, a6: $5d.a2}}), Control_Monad_Dom_Ref_mkNodes($0, $1.a6));
   }
  }
  case 1: return $0.a1.a1.a2(undefined)({a1: {h: 0}, a2: $1});
  case 2: return $0.a1.a1.a2(undefined)({a1: {h: 0}, a2: $1});
 }
}

function Control_Monad_Dom_Interface_positionStr($0) {
 switch($0) {
  case 0: return 'beforebegin';
  case 1: return 'afterbegin';
  case 2: return 'beforeend';
  case 3: return 'afterend';
 }
}

function Control_Monad_Dom_Event_toMouseInfo($0) {
 return Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_6(), $11 => $12 => $13 => ({a1: $11, a2: $12, a3: $13})), Control_Monad_Dom_Event_etId(csegen_34(), Builtin_believe_me($0))), Web_Raw_UIEvents_MouseEvent_shiftKey($0)), Web_Raw_UIEvents_MouseEvent_metaKey($0));
}

function Control_Monad_Dom_Event_etId($0, $1) {
 return $0.a2(undefined)($7 => Control_Monad_Dom_Event_prim__etId($1, $7));
}

function Web_Raw_UIEvents_MouseEvent_shiftKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), 'MouseEvent.shiftKey', $8 => Web_Internal_UIEventsPrim_MouseEvent_prim__shiftKey(Builtin_believe_me($0), $8));
}

function Web_Raw_UIEvents_MouseEvent_metaKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), 'MouseEvent.metaKey', $8 => Web_Internal_UIEventsPrim_MouseEvent_prim__metaKey(Builtin_believe_me($0), $8));
}

function Control_Monad_Dom_DomIO_unique_MonadDom_x28DomIOx20x24iox29($0, $1) {
 return $0.a1.a1.a2(undefined)(undefined)($0.a1.a2(undefined)($12 => ($1.a1.value)))(n => Prelude_Interfaces_x3ex3e($0.a1.a1, $0.a1.a2(undefined)($23 => ($1.a1.value=Prelude_Types_x2b_Num_Nat(n, 1n))), () => $0.a1.a1.a1.a2(undefined)(n)));
}

function Control_Monad_Dom_DomIO_text_MonadDom_x28DomIOx20x24iox29($0, $1, $2, $3) {
 const $54 = $55 => $56 => {
  switch($55.h) {
   case undefined: return Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, JS_Attribute_set(Web_Raw_Dom_Node_textContent($55.a1), $2), $56);
   case 0: return Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, 0, $56);
  }
 };
 return Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0.a1.a1, Control_Monad_Dom_DomIO_lookupRefAsElement({a1: {a1: {a1: {a1: b => a => func => $f => $10 => Control_Monad_Dom_DomIO_map_Functor_x28DomIOx20x24iox29($0.a1.a1.a1.a1, func, $f, $10), a2: a => $1b => $1c => Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, $1b, $1c), a3: b => a => $25 => $26 => $27 => Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, $25, $26, $27)}, a2: b => a => $31 => $32 => $33 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0.a1.a1, $31, $32, $33), a3: a => $3c => $3d => Control_Monad_Dom_DomIO_join_Monad_x28DomIOx20x24iox29($0.a1.a1, $3c, $3d)}, a2: a => $45 => $46 => Control_Monad_Dom_DomIO_liftIO_HasIO_x28DomIOx20x24iox29($0.a1, $45, $46)}, a2: a => $4d => $4e => Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, $4d, $4e)}, $1), $54, $3);
}

function Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0, $1, $2) {
 return $0.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_map_Functor_x28DomIOx20x24iox29($0, $1, $2, $3) {
 return $0(undefined)(undefined)($1)($2($3));
}

function Control_Monad_Dom_DomIO_listenTo_MonadDom_x28DomIOx20x24iox29($0, $1, $2) {
 return $0.a2(undefined)(Control_Monad_Dom_DomIO_listenRefImpl($2.a2, $1));
}

function Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, $1, $2) {
 return $0.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_liftIO_HasIO_x28DomIOx20x24iox29($0, $1, $2) {
 return $0.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_join_Monad_x28DomIOx20x24iox29($0, $1, $2) {
 return Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0, $1, $7 => $7, $2);
}

function Control_Monad_Dom_DomIO_insertAdjacent_MonadDom_x28DomIOx20x24iox29($0, $1, $2, $3, $4) {
 const $55 = $56 => $57 => {
  switch($56.h) {
   case undefined: return Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, Web_Raw_Dom_Element_insertAdjacentHTML($56.a1, Control_Monad_Dom_Interface_positionStr($2), Text_Html_Node_render($3)), $57);
   case 0: return Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, 0, $57);
  }
 };
 return Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0.a1.a1, Control_Monad_Dom_DomIO_lookupRefAsElement({a1: {a1: {a1: {a1: b => a => func => $10 => $11 => Control_Monad_Dom_DomIO_map_Functor_x28DomIOx20x24iox29($0.a1.a1.a1.a1, func, $10, $11), a2: a => $1c => $1d => Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, $1c, $1d), a3: b => a => $26 => $27 => $28 => Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, $26, $27, $28)}, a2: b => a => $32 => $33 => $34 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0.a1.a1, $32, $33, $34), a3: a => $3d => $3e => Control_Monad_Dom_DomIO_join_Monad_x28DomIOx20x24iox29($0.a1.a1, $3d, $3e)}, a2: a => $46 => $47 => Control_Monad_Dom_DomIO_liftIO_HasIO_x28DomIOx20x24iox29($0.a1, $46, $47)}, a2: a => $4e => $4f => Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, $4e, $4f)}, $1), $55, $4);
}

function Control_Monad_Dom_DomIO_innerHtml_MonadDom_x28DomIOx20x24iox29($0, $1, $2, $3) {
 const $54 = $55 => $56 => {
  switch($55.h) {
   case undefined: return Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, JS_Attribute_set(Web_Raw_Dom_InnerHTML_innerHTML($55.a1), Text_Html_Node_render($2)), $56);
   case 0: return Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, 0, $56);
  }
 };
 return Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0.a1.a1, Control_Monad_Dom_DomIO_lookupRefAsElement({a1: {a1: {a1: {a1: b => a => func => $f => $10 => Control_Monad_Dom_DomIO_map_Functor_x28DomIOx20x24iox29($0.a1.a1.a1.a1, func, $f, $10), a2: a => $1b => $1c => Control_Monad_Dom_DomIO_pure_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, $1b, $1c), a3: b => a => $25 => $26 => $27 => Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28DomIOx20x24iox29($0.a1.a1.a1, $25, $26, $27)}, a2: b => a => $31 => $32 => $33 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0.a1.a1, $31, $32, $33), a3: a => $3c => $3d => Control_Monad_Dom_DomIO_join_Monad_x28DomIOx20x24iox29($0.a1.a1, $3c, $3d)}, a2: a => $45 => $46 => Control_Monad_Dom_DomIO_liftIO_HasIO_x28DomIOx20x24iox29($0.a1, $45, $46)}, a2: a => $4d => $4e => Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28DomIOx20x24iox29($0, $4d, $4e)}, $1), $54, $3);
}

function Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28DomIOx20x24iox29($0, $1, $2, $3) {
 return $0.a2(undefined)(undefined)($1($3))($f => $2($f)($3));
}

function Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28DomIOx20x24iox29($0, $1, $2, $3) {
 return $0.a3(undefined)(undefined)($1($3))($2($3));
}

function Control_Monad_Dom_DomIO_lookupRefAsElement($0, $1) {
 return $0.a2(undefined)(Web_Dom_getElementById($1.a2));
}

function Control_Monad_Dom_DomIO_listenRefImpl($0, $1) {
 const $e = $f => {
  switch($f.h) {
   case undefined: return Prelude_Interfaces_traverse_({a1: {a1: acc => elem => func => init => input => Prelude_Types_foldr_Foldable_List(func, init, input), a2: elem => acc => func => init => input => Prelude_Types_foldl_Foldable_List(func, init, input), a3: elem => $1f => Prelude_Types_null_Foldable_List($1f), a4: elem => acc => m => $23 => funcM => init => input => Prelude_Types_foldlM_Foldable_List($23, funcM, init, input), a5: elem => $2a => Prelude_Types_toList_Foldable_List($2a), a6: a => m => $2e => f => $2f => Prelude_Types_foldMap_Foldable_List($2e, f, $2f)}, a2: csegen_21()}, $37 => Control_Monad_Dom_DomIO_listenImpl($0, $f.a1, $37), $1.a3);
   case 0: return csegen_37();
  }
 };
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Web_Dom_castElementById_($9 => $a => Web_Internal_HtmlTypes_safeCast_SafeCast_HTMLElement($a), $1.a2), $e);
}

function Control_Monad_Dom_DomIO_listenImpl($0, $1, $2) {
 switch($2) {
  case 0: return JS_Attribute_x21x3e($6 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($6), Web_Raw_Html_GlobalEventHandlers_onclick($1), e => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Control_Monad_Dom_Event_toMouseInfo(e), $14 => $0({h: 0, a1: $14})));
  case 1: return JS_Attribute_x21x3e($1a => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($1a), Web_Raw_Html_GlobalEventHandlers_ondblclick($1), e => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_12(), Control_Monad_Dom_Event_toMouseInfo(e), $28 => $0({h: 1, a1: $28})));
 }
}

function Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($0) {
 return Web_Raw_Html_MouseEventHandler_toMouseEventHandler($3 => $4 => JS_Util_runJS($0($3), $4));
}

function Web_Raw_Html_MouseEventHandler_toMouseEventHandler($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_31(), $5 => Web_Internal_HtmlPrim_MouseEventHandler_prim__toMouseEventHandler($0, $5));
}

function Web_Raw_Html_GlobalEventHandlers_ondblclick($0) {
 return JS_Attribute_fromNullablePrim(csegen_210(), 'GlobalEventHandlers.getondblclick', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__ondblclick($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOndblclick($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onclick($0) {
 return JS_Attribute_fromNullablePrim(csegen_210(), 'GlobalEventHandlers.getonclick', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onclick($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnclick($c, $d, $e), Builtin_believe_me($0));
}

function Data_IORef_newIORef($0, $1) {
 return $0.a1.a2(undefined)(undefined)($0.a2(undefined)($10 => ({value:$1})))(m => $0.a1.a1.a2(undefined)(m));
}

function Data_MSF_n__4310_2375_g($0, $1, $2) {
 const $4 = $1($2.a1)($2.a2);
 return {a1: $4, a2: $4};
}

function Data_MSF_step($0, $1, $2) {
 switch($2.h) {
  case 1: return $0.a1.a2(undefined)({a1: $2.a1, a2: $2});
  case 0: return $0.a1.a2(undefined)({a1: $1, a2: {h: 0}});
  case 2: return $0.a1.a2(undefined)({a1: $2.a1($1), a2: $2});
  case 3: return Prelude_Interfaces_x3cx24x3e($0.a1.a1, $23 => ({a1: $23, a2: $2}), $2.a1($1));
  case 4: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($36 => $0.a2(undefined)(undefined)(Data_MSF_step($0, $36.a1, $2.a2))($45 => $0.a1.a2(undefined)({a1: $45.a1, a2: {h: 4, a1: $36.a2, a2: $45.a2}})));
  case 5: {
   const $5d = $5e => {
    switch($5e.a1.h) {
     case undefined: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $5e.a1.a1, $2.a2))($6e => $0.a1.a2(undefined)({a1: $6e.a1, a2: {h: 5, a1: $5e.a2, a2: $6e.a2}}));
     case 0: return $0.a1.a2(undefined)({a1: {h: 0}, a2: {h: 5, a1: $5e.a2, a2: $2.a2}});
    }
   };
   return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($5d);
  }
  default: {
   switch($1.h) {
    case undefined: {
     switch($2.h) {
      case 6: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1.a1, $2.a1))($93 => $0.a2(undefined)(undefined)(Data_MSF_step($0, $1.a2, $2.a2))($a2 => $0.a1.a2(undefined)({a1: {a1: $93.a1, a2: $a2.a1}, a2: {h: 6, a1: $93.a2, a2: $a2.a2}})));
      default: {
       switch($2.h) {
        case 7: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($be => $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a2))($cd => $0.a1.a2(undefined)({a1: {a1: $be.a1, a2: $cd.a1}, a2: {h: 7, a1: $be.a2, a2: $cd.a2}})));
        default: {
         switch($1.h) {
          case 0: {
           switch($2.h) {
            case 8: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1.a1, $2.a1))($ea => $0.a1.a2(undefined)({a1: {h: 0, a1: $ea.a1}, a2: {h: 8, a1: $ea.a2, a2: $2.a2}}));
            default: {
             switch($2.h) {
              case 9: return $0.a2(undefined)(undefined)(Data_MSF_step($0, {a1: $1, a2: $2.a1}, $2.a2))($107 => $0.a1.a2(undefined)({a1: $107.a1.a1, a2: {h: 9, a1: $107.a1.a2, a2: $107.a2}}));
              case 11: return $0.a2(undefined)(undefined)($2.a2(undefined)($121 => Data_MSF_step($2.a1, $121, $2.a3))($1))($128 => $0.a1.a2(undefined)({a1: $128.a1, a2: {h: 11, a1: $2.a1, a2: $134 => $2.a2(undefined), a3: $128.a2}}));
              case 10: {
               const $144 = $145 => {
                switch($145.a1.a2.h) {
                 case undefined: {
                  const $149 = $2.a2($145.a1.a2.a1)($145.a1.a1);
                  return $0.a1.a2(undefined)({a1: $149.a1, a2: {h: 10, a1: $149.a2, a2: $2.a2}});
                 }
                 case 0: return $0.a1.a2(undefined)({a1: $145.a1.a1, a2: {h: 10, a1: $145.a2, a2: $2.a2}});
                }
               };
               return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($144);
              }
             }
            }
           }
          }
          case 1: {
           switch($2.h) {
            case 8: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1.a1, $2.a2))($170 => $0.a1.a2(undefined)({a1: {h: 1, a1: $170.a1}, a2: {h: 8, a1: $2.a1, a2: $170.a2}}));
            default: {
             switch($2.h) {
              case 9: return $0.a2(undefined)(undefined)(Data_MSF_step($0, {a1: $1, a2: $2.a1}, $2.a2))($18d => $0.a1.a2(undefined)({a1: $18d.a1.a1, a2: {h: 9, a1: $18d.a1.a2, a2: $18d.a2}}));
              case 11: return $0.a2(undefined)(undefined)($2.a2(undefined)($1a7 => Data_MSF_step($2.a1, $1a7, $2.a3))($1))($1ae => $0.a1.a2(undefined)({a1: $1ae.a1, a2: {h: 11, a1: $2.a1, a2: $1ba => $2.a2(undefined), a3: $1ae.a2}}));
              case 10: {
               const $1ca = $1cb => {
                switch($1cb.a1.a2.h) {
                 case undefined: {
                  const $1cf = $2.a2($1cb.a1.a2.a1)($1cb.a1.a1);
                  return $0.a1.a2(undefined)({a1: $1cf.a1, a2: {h: 10, a1: $1cf.a2, a2: $2.a2}});
                 }
                 case 0: return $0.a1.a2(undefined)({a1: $1cb.a1.a1, a2: {h: 10, a1: $1cb.a2, a2: $2.a2}});
                }
               };
               return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($1ca);
              }
             }
            }
           }
          }
          default: {
           switch($2.h) {
            case 9: return $0.a2(undefined)(undefined)(Data_MSF_step($0, {a1: $1, a2: $2.a1}, $2.a2))($1f8 => $0.a1.a2(undefined)({a1: $1f8.a1.a1, a2: {h: 9, a1: $1f8.a1.a2, a2: $1f8.a2}}));
            case 11: return $0.a2(undefined)(undefined)($2.a2(undefined)($212 => Data_MSF_step($2.a1, $212, $2.a3))($1))($219 => $0.a1.a2(undefined)({a1: $219.a1, a2: {h: 11, a1: $2.a1, a2: $225 => $2.a2(undefined), a3: $219.a2}}));
            case 10: {
             const $235 = $236 => {
              switch($236.a1.a2.h) {
               case undefined: {
                const $23a = $2.a2($236.a1.a2.a1)($236.a1.a1);
                return $0.a1.a2(undefined)({a1: $23a.a1, a2: {h: 10, a1: $23a.a2, a2: $2.a2}});
               }
               case 0: return $0.a1.a2(undefined)({a1: $236.a1.a1, a2: {h: 10, a1: $236.a2, a2: $2.a2}});
              }
             };
             return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($235);
            }
           }
          }
         }
        }
       }
      }
     }
    }
    default: {
     switch($2.h) {
      case 7: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($261 => $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a2))($270 => $0.a1.a2(undefined)({a1: {a1: $261.a1, a2: $270.a1}, a2: {h: 7, a1: $261.a2, a2: $270.a2}})));
      default: {
       switch($1.h) {
        case 0: {
         switch($2.h) {
          case 8: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1.a1, $2.a1))($28d => $0.a1.a2(undefined)({a1: {h: 0, a1: $28d.a1}, a2: {h: 8, a1: $28d.a2, a2: $2.a2}}));
          default: {
           switch($2.h) {
            case 9: return $0.a2(undefined)(undefined)(Data_MSF_step($0, {a1: $1, a2: $2.a1}, $2.a2))($2aa => $0.a1.a2(undefined)({a1: $2aa.a1.a1, a2: {h: 9, a1: $2aa.a1.a2, a2: $2aa.a2}}));
            case 11: return $0.a2(undefined)(undefined)($2.a2(undefined)($2c4 => Data_MSF_step($2.a1, $2c4, $2.a3))($1))($2cb => $0.a1.a2(undefined)({a1: $2cb.a1, a2: {h: 11, a1: $2.a1, a2: $2d7 => $2.a2(undefined), a3: $2cb.a2}}));
            case 10: {
             const $2e7 = $2e8 => {
              switch($2e8.a1.a2.h) {
               case undefined: {
                const $2ec = $2.a2($2e8.a1.a2.a1)($2e8.a1.a1);
                return $0.a1.a2(undefined)({a1: $2ec.a1, a2: {h: 10, a1: $2ec.a2, a2: $2.a2}});
               }
               case 0: return $0.a1.a2(undefined)({a1: $2e8.a1.a1, a2: {h: 10, a1: $2e8.a2, a2: $2.a2}});
              }
             };
             return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($2e7);
            }
           }
          }
         }
        }
        case 1: {
         switch($2.h) {
          case 8: return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1.a1, $2.a2))($313 => $0.a1.a2(undefined)({a1: {h: 1, a1: $313.a1}, a2: {h: 8, a1: $2.a1, a2: $313.a2}}));
          default: {
           switch($2.h) {
            case 9: return $0.a2(undefined)(undefined)(Data_MSF_step($0, {a1: $1, a2: $2.a1}, $2.a2))($330 => $0.a1.a2(undefined)({a1: $330.a1.a1, a2: {h: 9, a1: $330.a1.a2, a2: $330.a2}}));
            case 11: return $0.a2(undefined)(undefined)($2.a2(undefined)($34a => Data_MSF_step($2.a1, $34a, $2.a3))($1))($351 => $0.a1.a2(undefined)({a1: $351.a1, a2: {h: 11, a1: $2.a1, a2: $35d => $2.a2(undefined), a3: $351.a2}}));
            case 10: {
             const $36d = $36e => {
              switch($36e.a1.a2.h) {
               case undefined: {
                const $372 = $2.a2($36e.a1.a2.a1)($36e.a1.a1);
                return $0.a1.a2(undefined)({a1: $372.a1, a2: {h: 10, a1: $372.a2, a2: $2.a2}});
               }
               case 0: return $0.a1.a2(undefined)({a1: $36e.a1.a1, a2: {h: 10, a1: $36e.a2, a2: $2.a2}});
              }
             };
             return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($36d);
            }
           }
          }
         }
        }
        default: {
         switch($2.h) {
          case 9: return $0.a2(undefined)(undefined)(Data_MSF_step($0, {a1: $1, a2: $2.a1}, $2.a2))($39b => $0.a1.a2(undefined)({a1: $39b.a1.a1, a2: {h: 9, a1: $39b.a1.a2, a2: $39b.a2}}));
          case 11: return $0.a2(undefined)(undefined)($2.a2(undefined)($3b5 => Data_MSF_step($2.a1, $3b5, $2.a3))($1))($3bc => $0.a1.a2(undefined)({a1: $3bc.a1, a2: {h: 11, a1: $2.a1, a2: $3c8 => $2.a2(undefined), a3: $3bc.a2}}));
          case 10: {
           const $3d8 = $3d9 => {
            switch($3d9.a1.a2.h) {
             case undefined: {
              const $3dd = $2.a2($3d9.a1.a2.a1)($3d9.a1.a1);
              return $0.a1.a2(undefined)({a1: $3dd.a1, a2: {h: 10, a1: $3dd.a2, a2: $2.a2}});
             }
             case 0: return $0.a1.a2(undefined)({a1: $3d9.a1.a1, a2: {h: 10, a1: $3d9.a2, a2: $2.a2}});
            }
           };
           return $0.a2(undefined)(undefined)(Data_MSF_step($0, $1, $2.a1))($3d8);
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
}

function Data_MSF_accumulateWith($0, $1) {
 return {h: 9, a1: $1, a2: {h: 2, a1: $5 => Data_MSF_n__4310_2375_g($1, $0, $5)}};
}

function Data_Event_pure_Applicative_Event($0) {
 return {a1: $0};
}

function Data_Event_map_Functor_Event($0, $1) {
 switch($1.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0($1.a1)};
 }
}

function Data_Event_unionL($0, $1) {
 switch($0.h) {
  case undefined: return {a1: $0.a1};
  default: return $1;
 }
}

function Data_Event_maybeToEvent($0) {
 switch($0.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0.a1};
 }
}

function Control_Category_x3ex3ex3e($0, $1, $2) {
 return $0.a2(undefined)(undefined)(undefined)($2)($1);
}

const Data_MES_id_Category_x28MESx20x24mx29 = __lazy(function () {
 return {h: 2, a1: $1 => Data_Event_pure_Applicative_Event($1)};
});

function Data_MES_x2e_Category_x28MESx20x24mx29($0, $1) {
 return {h: 5, a1: $1, a2: $0};
}

function Data_MES_when($0) {
 return {h: 2, a1: $2 => Data_Event_maybeToEvent($0($2))};
}

function Data_MES_unionL($0, $1) {
 return {h: 4, a1: {h: 7, a1: Prelude_Interfaces_x3cx24x3e($6 => $7 => $8 => $9 => Control_Category_x3ex3ex3e(csegen_107(), $9, {h: 2, a1: $8}), $11 => $12 => Data_Event_unionL($11, $12), $0), a2: $1}, a2: {h: 2, a1: $1a => Prelude_Basics_uncurry($1d => $1e => Prelude_Basics_apply($1d, $1e), $1a)}};
}

function Data_MES_onWith($0, $1, $2) {
 return {h: 4, a1: {h: 7, a1: $1, a2: $2}, a2: {h: 2, a1: $8 => Prelude_Interfaces_x3cx24x3e($c => $d => $e => $f => Data_Event_map_Functor_Event($e, $f), $0($8.a1), $8.a2)}};
}

function Data_MES_on($0, $1) {
 return Data_MES_onWith($4 => $5 => $4, $0, $1);
}


try{__mainExpression_0()}catch(e){if(e instanceof IdrisError){console.log('ERROR: ' + e.message)}else{throw e} }
