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
const Web_Internal_DomPrim_InnerHTML_prim__setInnerHTML = ((x,v)=>{x.innerHTML = v});
const Web_Internal_DomPrim_Element_prim__setAttribute = ((x,a,b)=>x.setAttribute(a,b));
const Web_Internal_DomPrim_Element_prim__removeAttribute = ((x,a)=>x.removeAttribute(a));
const Web_Internal_DomPrim_InnerHTML_prim__innerHTML = (x=>x.innerHTML);
const Web_Internal_DomPrim_NonElementParentNode_prim__getElementById = ((x,a)=>x.getElementById(a));
const Web_Internal_HtmlPrim_HTMLInputElement_prim__value = (x=>x.value);
const Web_Internal_HtmlPrim_MouseEventHandler_prim__toMouseEventHandler = (x=>(a)=>x(a)());
const Web_Internal_HtmlPrim_KeyboardEventHandler_prim__toKeyboardEventHandler = (x=>(a)=>x(a)());
const Web_Internal_HtmlPrim_InputEventHandler_prim__toInputEventHandler = (x=>(a)=>x(a)());
const Web_Internal_HtmlPrim_EventHandlerNonNull_prim__toEventHandlerNonNull = (x=>(a)=>x(a)());
const Web_Internal_HtmlPrim_HTMLInputElement_prim__setValue = ((x,v)=>{x.value = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseup = ((x,v)=>{x.onmouseup = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseover = ((x,v)=>{x.onmouseover = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseout = ((x,v)=>{x.onmouseout = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmousemove = ((x,v)=>{x.onmousemove = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseleave = ((x,v)=>{x.onmouseleave = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseenter = ((x,v)=>{x.onmouseenter = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmousedown = ((x,v)=>{x.onmousedown = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnkeyup = ((x,v)=>{x.onkeyup = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnkeydown = ((x,v)=>{x.onkeydown = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOninput = ((x,v)=>{x.oninput = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOndblclick = ((x,v)=>{x.ondblclick = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnclick = ((x,v)=>{x.onclick = v});
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnchange = ((x,v)=>{x.onchange = v});
const Web_Internal_HtmlPrim_HTMLInputElement_prim__setCustomValidity = ((x,a)=>x.setCustomValidity(a));
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseup = (x=>x.onmouseup);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseover = (x=>x.onmouseover);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseout = (x=>x.onmouseout);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmousemove = (x=>x.onmousemove);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseleave = (x=>x.onmouseleave);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseenter = (x=>x.onmouseenter);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmousedown = (x=>x.onmousedown);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onkeyup = (x=>x.onkeyup);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onkeydown = (x=>x.onkeydown);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__oninput = (x=>x.oninput);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__ondblclick = (x=>x.ondblclick);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onclick = (x=>x.onclick);
const Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onchange = (x=>x.onchange);
const Control_Monad_Dom_Event_prim__input = (x=>x.target.value || x.target.innerHTML || '');
const Web_Internal_UIEventsPrim_MouseEvent_prim__shiftKey = (x=>x.shiftKey);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__shiftKey = (x=>x.shiftKey);
const Web_Internal_UIEventsPrim_MouseEvent_prim__screenY = (x=>x.screenY);
const Web_Internal_UIEventsPrim_MouseEvent_prim__screenX = (x=>x.screenX);
const Web_Internal_UIEventsPrim_MouseEvent_prim__metaKey = (x=>x.metaKey);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__metaKey = (x=>x.metaKey);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__location = (x=>x.location);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__key = (x=>x.key);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__isComposing = (x=>x.isComposing);
const Web_Internal_UIEventsPrim_MouseEvent_prim__ctrlKey = (x=>x.ctrlKey);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__ctrlKey = (x=>x.ctrlKey);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__code = (x=>x.code);
const Web_Internal_UIEventsPrim_MouseEvent_prim__clientY = (x=>x.clientY);
const Web_Internal_UIEventsPrim_MouseEvent_prim__clientX = (x=>x.clientX);
const Web_Internal_UIEventsPrim_MouseEvent_prim__buttons = (x=>x.buttons);
const Web_Internal_UIEventsPrim_MouseEvent_prim__button = (x=>x.button);
const Web_Internal_UIEventsPrim_MouseEvent_prim__altKey = (x=>x.altKey);
const Web_Internal_UIEventsPrim_KeyboardEvent_prim__altKey = (x=>x.altKey);
const Examples_Performance_prim__time = ((w) => new Date().getTime());
const Examples_Fractals_prim__setInterval = ((n,h,w)=>setInterval(() => h(w),n));
const Examples_Fractals_prim__clearInterval = ((i,w)=>clearInterval(i));
function x24tcOpt_8($0) {
 switch($0.a1.h) {
  case undefined: {
   switch($0.a1.a1.h) {
    case 0: return {h: 0, a1: {a1: $0.a1.a1.a1}};
    case 1: {
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
 return __tailRec(x24tcOpt_8, {h: 1, a1: $0});
}

function x24tcOpt_5($0) {
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
 return __tailRec(x24tcOpt_5, {h: 1, a1: $0, a2: $1});
}

function x24tcOpt_7($0) {
 switch($0.a2.h) {
  case 0: return {h: 0, a1: $0.a1};
  case undefined: return {h: 1, a1: {a1: $0.a2.a1, a2: $0.a1}, a2: $0.a2.a2};
 }
}

function Prelude_Types_List_reverseOnto($0, $1) {
 return __tailRec(x24tcOpt_7, {h: 1, a1: $0, a2: $1});
}

function x24tcOpt_2($0) {
 switch($0.a1.h) {
  case 0: return {h: 0, a1: 1};
  case undefined: {
   switch($0.a1.a1.a1($0.a2.a1)($0.a3.a1)) {
    case 1: return {h: 1, a1: $0.a1.a2, a2: $0.a2.a2, a3: $0.a3.a2};
    case 0: return {h: 0, a1: 0};
   }
  }
 }
}

function Data_SOP_NP_x3dx3d_Eq_x28x28x28NP_x20x24kx29x20x24fx29x20x24ksx29($0, $1, $2) {
 return __tailRec(x24tcOpt_2, {h: 1, a1: $0, a2: $1, a3: $2});
}

function x24tcOpt_10($0) {
 switch($0.a2.h) {
  case 0: {
   switch($0.a3.h) {
    case 0: return {h: 0, a1: $0.a1.a1.a1($0.a2.a1)($0.a3.a1)};
    default: return {h: 0, a1: 0};
   }
  }
  case 1: {
   switch($0.a3.h) {
    case 1: return {h: 1, a1: $0.a1.a2, a2: $0.a2.a1, a3: $0.a3.a1};
    default: return {h: 0, a1: 0};
   }
  }
  default: return {h: 0, a1: 0};
 }
}

function Data_SOP_NS_x3dx3d_Eq_x28x28x28NS_x20x24kx29x20x24fx29x20x24ksx29($0, $1, $2) {
 return __tailRec(x24tcOpt_10, {h: 1, a1: $0, a2: $1, a3: $2});
}

function x24tcOpt_9($0) {
 switch($0.a2.h) {
  case 0: return {h: 0, a1: $0.a1};
  case undefined: {
   switch($0.a2.a1.h) {
    case 3: return {h: 1, a1: {a1: $0.a2.a1.a1, a2: $0.a1}, a2: $0.a2.a2};
    default: return {h: 1, a1: $0.a1, a2: $0.a2.a2};
   }
  }
 }
}

function Text_Html_Attribute_n__3149_1133_go($0, $1) {
 return __tailRec(x24tcOpt_9, {h: 1, a1: $0, a2: $1});
}

function x24tcOpt_1($0) {
 switch($0.a3.h) {
  case undefined: return {h: 1, a1: $0.a1, a2: {a1: Text_Html_Node_render($0.a3.a1), a2: $0.a2}, a3: $0.a3.a2};
  case 0: return {h: 0, a1: Prelude_Types_fastConcat(Prelude_Types_List_reverse($0.a2))};
 }
}

function Text_Html_Node_n__11072_2894_go($0, $1, $2) {
 return __tailRec(x24tcOpt_1, {h: 1, a1: $0, a2: $1, a3: $2});
}

function x24tcOpt_6($0) {
 switch($0.a4) {
  case 0n: return {h: 0, a1: Prelude_Types_List_reverse($0.a3)};
  default: {
   const $6 = ($0.a4-1n);
   return {h: 1, a1: $0.a1, a2: $0.a2, a3: {a1: $0.a5, a2: $0.a3}, a4: $6, a5: $0.a1($0.a5)};
  }
 }
}

function Data_List_TR_n__3136_2253_run($0, $1, $2, $3, $4) {
 return __tailRec(x24tcOpt_6, {h: 1, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4});
}

function x24tcOpt_4($0) {
 switch($0.a3.h) {
  case 0: return {h: 0, a1: Prelude_Types_List_reverse($0.a2)};
  case undefined: return {h: 1, a1: $0.a1, a2: {a1: $0.a1($0.a3.a1), a2: $0.a2}, a3: $0.a3.a2};
 }
}

function Data_List_TR_n__3157_2277_run($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 1, a1: $0, a2: $1, a3: $2});
}

function x24tcOpt_3($0) {
 const $2 = $0.a3($0.a4)($0.a5)($0.a6);
 switch($2.h) {
  case 1: return {h: 0, a1: $2.a1};
  case 0: return {h: 1, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $2.a1, a5: $2.a2, a6: undefined};
 }
}

function Control_MonadRec_n__5335_2172_run($0, $1, $2, $3, $4, $5) {
 return __tailRec(x24tcOpt_3, {h: 1, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5});
}

const __mainExpression_0 = __lazy(function () {
 return PrimIO_unsafePerformIO($2 => Examples_Main_main($2));
});

const csegen_2 = __lazy(function () {
 return $0 => $1 => $2 => $3 => $4 => Prelude_IO_map_Functor_IO($2, $3, $4);
});

const csegen_5 = __lazy(function () {
 return b => a => func => $0 => $1 => Prelude_IO_map_Functor_IO(func, $0, $1);
});

const csegen_9 = __lazy(function () {
 const $5 = b => a => $6 => $7 => $8 => {
  const $9 = $6($8);
  const $c = $7($8);
  return $9($c);
 };
 return {a1: csegen_5(), a2: a => $3 => $4 => $3, a3: $5};
});

const csegen_12 = __lazy(function () {
 return b => a => $0 => $1 => $2 => {
  const $3 = $0($2);
  return $1($3)($2);
 };
});

const csegen_15 = __lazy(function () {
 const $4 = a => $5 => $6 => {
  const $7 = $5($6);
  return $7($6);
 };
 return {a1: csegen_9(), a2: csegen_12(), a3: $4};
});

const csegen_18 = __lazy(function () {
 return b => a => func => $0 => Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29(csegen_5(), func, $0);
});

const csegen_24 = __lazy(function () {
 return {a1: csegen_18(), a2: a => $3 => Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $3), a3: b => a => $9 => $a => Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $9, $a)};
});

const csegen_27 = __lazy(function () {
 const $4 = a => $5 => $6 => {
  const $7 = $5($6);
  return $7($6);
 };
 return {a1: csegen_9(), a2: csegen_12(), a3: $4};
});

const csegen_33 = __lazy(function () {
 return {a1: csegen_24(), a2: b => a => $3 => $4 => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_27(), $3, $4), a3: a => $b => Control_Monad_Error_Either_join_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_27(), $b)};
});

const csegen_34 = __lazy(function () {
 return {a1: csegen_27(), a2: a => $3 => $3};
});

const csegen_37 = __lazy(function () {
 return {a1: csegen_33(), a2: a => $3 => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $3)};
});

const csegen_43 = __lazy(function () {
 return {a1: $1 => JS_Marshall_toFFI_ToFFI_String_String($1), a2: $5 => JS_Marshall_fromFFI_FromFFI_String_String($5)};
});

const csegen_57 = __lazy(function () {
 return {a1: $1 => Web_Internal_HtmlTypes_toFFI_ToFFI_MouseEventHandler_MouseEventHandler($1), a2: $5 => Web_Internal_HtmlTypes_fromFFI_FromFFI_MouseEventHandler_MouseEventHandler($5)};
});

const csegen_72 = __lazy(function () {
 return {a1: $1 => Web_Internal_HtmlTypes_toFFI_ToFFI_KeyboardEventHandler_KeyboardEventHandler($1), a2: $5 => Web_Internal_HtmlTypes_fromFFI_FromFFI_KeyboardEventHandler_KeyboardEventHandler($5)};
});

const csegen_98 = __lazy(function () {
 return $0 => $1 => Web_Internal_HtmlTypes_safeCast_SafeCast_HTMLElement($1);
});

const csegen_109 = __lazy(function () {
 return $0 => $1 => $2 => $3 => Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29(csegen_2(), $2, $3);
});

const csegen_118 = __lazy(function () {
 return {a1: b => a => func => $1 => $2 => Control_Monad_Dom_DomIO_map_Functor_x28x28DomIOx20x24evx29x20x24iox29(csegen_18(), func, $1, $2), a2: a => $a => $b => Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), $a, $b), a3: b => a => $12 => $13 => $14 => Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), $12, $13, $14)};
});

const csegen_124 = __lazy(function () {
 return {a1: csegen_24(), a2: b => a => $3 => $4 => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), $3, $4), a3: a => $b => Control_Monad_Error_Either_join_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), $b)};
});

const csegen_130 = __lazy(function () {
 return {a1: csegen_118(), a2: b => a => $3 => $4 => $5 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_124(), $3, $4, $5), a3: a => $d => $e => Control_Monad_Dom_DomIO_join_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_124(), $d, $e)};
});

const csegen_131 = __lazy(function () {
 return Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), 0);
});

const csegen_149 = __lazy(function () {
 return {a1: {a1: acc => elem => func => init => input => Prelude_Types_foldr_Foldable_Maybe(func, init, input), a2: elem => acc => func => init => input => Prelude_Types_foldl_Foldable_Maybe(func, init, input), a3: elem => $c => Prelude_Types_null_Foldable_Maybe($c), a4: elem => acc => m => $10 => funcM => init => input => Prelude_Types_foldlM_Foldable_Maybe($10, funcM, init, input), a5: elem => $17 => Prelude_Types_toList_Foldable_Maybe($17), a6: a => m => $1b => f => $1c => Prelude_Types_foldMap_Foldable_Maybe($1b, f, $1c)}, a2: csegen_24()};
});

const csegen_153 = __lazy(function () {
 return $0 => $1 => Web_Internal_HtmlTypes_safeCast_SafeCast_HTMLInputElement($1);
});

const csegen_156 = __lazy(function () {
 return {a1: $1 => $2 => Prelude_Interfaces_x3cx2bx3e_Semigroup_x28x7cUnitx2cMkUnitx7cx29($1, $2), a2: Prelude_Interfaces_neutral_Monoid_x28x7cUnitx2cMkUnitx7cx29()};
});

const csegen_164 = __lazy(function () {
 return {a1: csegen_118(), a2: b => a => $3 => $4 => $5 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_33(), $3, $4, $5), a3: a => $d => $e => Control_Monad_Dom_DomIO_join_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_33(), $d, $e)};
});

const csegen_167 = __lazy(function () {
 return {a1: csegen_164(), a2: a => $3 => $4 => Control_Monad_Dom_DomIO_liftIO_HasIO_x28x28DomIOx20x24evx29x20x24iox29(csegen_37(), $3, $4)};
});

const csegen_168 = __lazy(function () {
 return {a1: csegen_37(), a2: a => $3 => $3};
});

const csegen_171 = __lazy(function () {
 return {a1: csegen_167(), a2: a => $3 => $4 => Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28x28DomIOx20x24evx29x20x24iox29(csegen_168(), $3, $4)};
});

const csegen_190 = __lazy(function () {
 return {a1: csegen_130(), a2: b => st => a => rel => step => seed => ini => prf => $3 => Control_Monad_Dom_DomIO_tailRecM_MonadRec_x28x28DomIOx20x24ex29x20x24iox29({a1: csegen_124(), a2: $9 => $a => $b => $c => $d => $e => $f => $10 => Control_MonadRec_tailRecM_MonadRec_x28x28EitherTx20x24ex29x20x24mx29({a1: csegen_15(), a2: $16 => $17 => $18 => $19 => $1a => $1b => $1c => $1d => $1e => Control_MonadRec_trIO($1a, $1b, $1c, $1e)}, $d, $e, $f)}, step, seed, ini, $3)};
});

const csegen_193 = __lazy(function () {
 return {a1: csegen_164(), a2: $3 => Control_Monad_Dom_DomIO_createId($3), a3: t => $7 => $8 => $9 => Control_Monad_Dom_DomIO_registerImpl($7, $8, $9)};
});

const csegen_210 = __lazy(function () {
 return {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_widgetLabel()}, a2: {h: 0}};
});

const csegen_212 = __lazy(function () {
 return $0 => Prelude_Types_fastConcat(Data_List_intersperse(' ', $0));
});

const csegen_234 = __lazy(function () {
 return {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_widgetList()}, a2: {h: 0}};
});

const csegen_252 = __lazy(function () {
 return Rhone_JS_Sink_boolAttribute(csegen_171(), 'disabled');
});

const csegen_270 = __lazy(function () {
 return {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_widgetLine()}, a2: {h: 0}};
});

const csegen_280 = __lazy(function () {
 const $0 = Examples_CSS_Reset_out();
 return $0.a3;
});

const csegen_296 = __lazy(function () {
 return {a1: 19, a2: {h: 0, a1: {h: 0, a1: 5}}};
});

const csegen_297 = __lazy(function () {
 return {a1: {a1: 26, a2: {h: 1, a1: 10}}, a2: {h: 0}};
});

const csegen_310 = __lazy(function () {
 return {a1: 16, a2: {h: 1, a1: Number(_truncUBigInt32(100n))}};
});

const csegen_335 = __lazy(function () {
 return {a1: 3, a2: {h: 4, a1: Examples_CSS_Colors_base80()}};
});

const csegen_336 = __lazy(function () {
 return {a1: 19, a2: {h: 4, a1: {h: 1, a1: 40}}};
});

const csegen_358 = __lazy(function () {
 return {a1: 24, a2: {h: 0, a1: {h: 1, a1: 5}}};
});

const csegen_359 = __lazy(function () {
 return {a1: 19, a2: {h: 0, a1: {h: 1, a1: 5}}};
});

const csegen_360 = __lazy(function () {
 return {a1: csegen_359(), a2: {h: 0}};
});

const csegen_373 = __lazy(function () {
 return {a1: {a1: 2, a2: Examples_CSS_Colors_lightest_grey()}, a2: {a1: {a1: 3, a2: {h: 0, a1: Examples_CSS_Colors_comp60()}}, a2: {h: 0}}};
});

const csegen_382 = __lazy(function () {
 return {a1: {a1: 25, a2: 1}, a2: {h: 0}};
});

const csegen_402 = __lazy(function () {
 return {a1: {a1: 26, a2: {h: 1, a1: 20}}, a2: {h: 0}};
});

const csegen_456 = __lazy(function () {
 return {a1: {h: 0}, a2: {a1: {h: 0}, a2: {h: 0}}};
});

const csegen_461 = __lazy(function () {
 return {h: 2, a1: $1 => Data_Either_isLeft($1)};
});

const csegen_476 = __lazy(function () {
 const $0 = Examples_CSS_Performance_btnRun();
 return $0.a3;
});

const csegen_481 = __lazy(function () {
 return {a1: Text_Html_Attribute_dispAttr('class', csegen_212(), {a1: Examples_CSS_Core_widget(), a2: {a1: Examples_CSS_Core_btn(), a2: {h: 0}}}), a2: {h: 0}};
});

const csegen_488 = __lazy(function () {
 const $0 = Examples_CSS_Performance_out();
 return $0.a3;
});

const csegen_507 = __lazy(function () {
 return $0 => Control_Monad_Dom_DomIO_liftIO_HasIO_x28x28DomIOx20x24evx29x20x24iox29(csegen_37(), $5 => Examples_Performance_prim__time($5), $0);
});

const csegen_517 = __lazy(function () {
 return {a1: {a1: 25, a2: 1}, a2: csegen_402()};
});

const csegen_518 = __lazy(function () {
 return {a1: csegen_359(), a2: csegen_517()};
});

const csegen_569 = __lazy(function () {
 return x => 0;
});

const csegen_572 = __lazy(function () {
 return {h: 1, a1: {h: 1, a1: {h: 1, a1: {h: 0, a1: {h: 0}}}}};
});

const csegen_582 = __lazy(function () {
 return {a1: {a1: Examples_Fractals_implEqFractal(), a2: {h: 0}}, a2: {a1: {h: 0}, a2: {a1: {h: 0}, a2: csegen_456()}}};
});

const csegen_586 = __lazy(function () {
 const $0 = Examples_CSS_Fractals_txtIter();
 return $0.a3;
});

const csegen_599 = __lazy(function () {
 const $0 = Examples_CSS_Fractals_txtRedraw();
 return $0.a3;
});

const csegen_607 = __lazy(function () {
 const $0 = Examples_CSS_Fractals_btnRun();
 return $0.a3;
});

const csegen_616 = __lazy(function () {
 const $0 = Examples_CSS_Fractals_out();
 return $0.a3;
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

function Examples_Main_main($0) {
 return JS_Util_runJS(Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29(csegen_2(), $7 => 0, Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Data_IORef_newIORef(csegen_37(), Prelude_Types_fromInteger_Num_Nat(0n)), $14 => Control_Monad_Dom_DomIO_reactimateDom_({a1: 'reset'}, 'select', Examples_Selector_ui(), $14))), $0);
}

function Web_Dom_getElementById($0) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Web_Dom_document(), $7 => Web_Raw_Dom_NonElementParentNode_getElementById($7, $0));
}

const Web_Dom_document = __lazy(function () {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $4 => Web_Dom_prim__document($4));
});

function Web_Dom_castElementById_($0, $1) {
 return Control_Monad_Error_Either_map_Functor_x28x28EitherTx20x24ex29x20x24mx29(csegen_2(), $6 => Prelude_Types_x3ex3ex3d_Monad_Maybe($6, $a => $0(undefined)($a)), Web_Dom_getElementById($1));
}

function Web_Dom_castElementById($0, $1) {
 return Web_Dom_castElementById_($0, $1);
}

function Web_Raw_Dom_Element_setAttribute($0, $1, $2) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $7 => Web_Internal_DomPrim_Element_prim__setAttribute(Builtin_believe_me($0), $1, $2, $7));
}

function Web_Raw_Dom_Element_removeAttribute($0, $1) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $6 => Web_Internal_DomPrim_Element_prim__removeAttribute(Builtin_believe_me($0), $1, $6));
}

function Web_Raw_Dom_InnerHTML_innerHTML($0) {
 return JS_Attribute_fromPrim(csegen_43(), 'InnerHTML.getinnerHTML', $6 => $7 => Web_Internal_DomPrim_InnerHTML_prim__innerHTML($6, $7), $c => $d => $e => Web_Internal_DomPrim_InnerHTML_prim__setInnerHTML($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Dom_NonElementParentNode_getElementById($0, $1) {
 return JS_Marshall_tryJS($4 => JS_Nullable_fromFFI_FromFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29($7 => Web_Internal_DomTypes_fromFFI_FromFFI_Element_Element($7), $4), () => 'NonElementParentNode.getElementById', $d => Web_Internal_DomPrim_NonElementParentNode_prim__getElementById(Builtin_believe_me($0), $1, $d));
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

function Prelude_Types_toList_Foldable_Maybe($0) {
 return Prelude_Types_foldr_Foldable_Maybe($3 => $4 => ({a1: $3, a2: $4}), {h: 0}, $0);
}

function Prelude_Types_null_Foldable_Maybe($0) {
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

function Prelude_Types_fromInteger_Num_Nat($0) {
 return Prelude_Types_prim__integerToNat($0);
}

function Prelude_Types_foldr_Foldable_Maybe($0, $1, $2) {
 switch($2.h) {
  case 0: return $1;
  case undefined: return $0($2.a1)($1);
 }
}

function Prelude_Types_foldl_Foldable_Maybe($0, $1, $2) {
 return Prelude_Types_foldr_Foldable_Maybe($6 => $7 => Prelude_Basics_flip($a => $b => $c => $a($b($c)), $12 => Prelude_Basics_flip($0, $6, $12), $7), $19 => $19, $2)($1);
}

function Prelude_Types_foldlM_Foldable_Maybe($0, $1, $2, $3) {
 return Prelude_Types_foldl_Foldable_Maybe(ma => b => $0.a2(undefined)(undefined)(ma)($f => Prelude_Basics_flip($1, b, $f)), $0.a1.a2(undefined)($2), $3);
}

function Prelude_Types_foldMap_Foldable_Maybe($0, $1, $2) {
 return Prelude_Types_foldr_Foldable_Maybe($5 => $6 => $0.a1($1($5))($6), $0.a2, $2);
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

function Prelude_Types_x3dx3d_Eq_Nat($0, $1) {
 return (($0===$1)?1:0);
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

function Prelude_Types_pow($0, $1) {
 return Prelude_Types_exp(Prelude_Num_x2a_Num_Double($1, Prelude_Types_log($0)));
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

function Prelude_Types_log($0) {
 return Math.log($0);
}

function Prelude_Types_exp($0) {
 return Math.exp($0);
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

function Prelude_Num_x2f_Fractional_Double($0, $1) {
 return ($0/$1);
}

function Prelude_Num_x2a_Num_Double($0, $1) {
 return ($0*$1);
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

function Prelude_EqOrd_x3ex3d_Ord_Int8($0, $1) {
 switch((($0>=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
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

function Prelude_EqOrd_x3dx3d_Eq_Int8($0, $1) {
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

function Prelude_EqOrd_x3cx3d_Ord_Int8($0, $1) {
 switch((($0<=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

function Prelude_EqOrd_x3cx3d_Ord_Bits32($0, $1) {
 switch((($0<=$1)?1:0)) {
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

const Prelude_Interfaces_neutral_Monoid_x28x7cUnitx2cMkUnitx7cx29 = __lazy(function () {
 return 0;
});

function Prelude_Interfaces_x3cx2bx3e_Semigroup_x28x7cUnitx2cMkUnitx7cx29($0, $1) {
 return 0;
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

function Prelude_Interfaces_x24x3e($0, $1, $2) {
 return $0(undefined)(undefined)($a => $2)($1);
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

function Prelude_Show_show_Show_Int8($0) {
 return Prelude_Show_showPrec_Show_Int8({h: 0}, $0);
}

function Prelude_Show_show_Show_Int32($0) {
 return Prelude_Show_showPrec_Show_Int32({h: 0}, $0);
}

function Prelude_Show_show_Show_Double($0) {
 return Prelude_Show_showPrec_Show_Double({h: 0}, $0);
}

function Prelude_Show_show_Show_Bits32($0) {
 return Prelude_Show_showPrec_Show_Bits32({h: 0}, $0);
}

function Prelude_Show_show_Show_Bits16($0) {
 return Prelude_Show_showPrec_Show_Bits16({h: 0}, $0);
}

function Prelude_Show_showPrec_Show_Integer($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Int8($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Int32($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Double($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

function Prelude_Show_showPrec_Show_Bits32($0, $1) {
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

function Prelude_Cast_cast_Cast_String_Nat($0) {
 return Prelude_Types_prim__integerToNat(Prelude_Cast_cast_Cast_String_Integer($0));
}

function Prelude_Cast_cast_Cast_String_Integer($0) {
 return _bigIntOfString($0);
}

function Prelude_Cast_cast_Cast_String_Bits32($0) {
 return _truncUInt32(_intOfString($0));
}

function Prelude_Cast_cast_Cast_Nat_Double($0) {
 return Number($0);
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
 return JS_Util_runJSWith(() => $4 => JS_Util_consoleLog(csegen_34(), JS_Util_dispErr($4)), $0, $1);
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

function Data_Maybe_toMaybe($0, $1) {
 switch($0) {
  case 1: return {a1: $1()};
  case 0: return {h: 0};
 }
}

function Control_Monad_Error_Interface_throwError_MonadError_x24e_x28x28EitherTx20x24ex29x20x24mx29($0, $1) {
 return $0.a1.a2(undefined)({h: 0, a1: $1});
}

function Data_Bits_fromNat($0, $1) {
 return $0;
}

function Data_Nat_succNotLTEzero($0) {
 _crashExp('No clauses');
}

function Data_Nat_isLTE($0, $1) {
 switch($0) {
  case 0n: return {h: 0, a1: 0n};
  default: {
   const $4 = ($0-1n);
   switch($1) {
    case 0n: return {h: 1, a1: $9 => Data_Nat_succNotLTEzero($9)};
    default: {
     const $c = ($1-1n);
     const $f = Data_Nat_isLTE($4, $c);
     switch($f.h) {
      case 1: return {h: 1, a1: $14 => $f.a1(Data_Nat_fromLteSucc($14))};
      case 0: return {h: 0, a1: (1n+$f.a1)};
     }
    }
   }
  }
 }
}

function Data_Nat_fromLteSucc($0) {
 switch($0) {
  case 0n: _crashExp('Nat case not covered');
  default: {
   const $2 = ($0-1n);
   return $2;
  }
 }
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

function Data_Either_isLeft($0) {
 switch($0.h) {
  case 0: return 1;
  case 1: return 0;
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
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $2), $c => JS_Marshall_tryFromFFI($0, $1, $c));
}

function JS_Marshall_tryFromFFI($0, $1, $2) {
 const $3 = $0($2);
 switch($3.h) {
  case 0: return Control_Monad_Error_Interface_throwError_MonadError_x24e_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), {h: 1, a1: $1(), a2: $2});
  case undefined: return Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $3.a1);
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

function Data_SOP_SOP_x3dx3d_Eq_x28x28x28SOP_x20x24kx29x20x24fx29x20x24kssx29($0, $1, $2) {
 return Data_SOP_NS_x3dx3d_Eq_x28x28x28NS_x20x24kx29x20x24fx29x20x24ksx29(Data_SOP_POP_popToNP($0, ks => $8 => ({a1: $a => $b => Data_SOP_NP_x3dx3d_Eq_x28x28x28NP_x20x24kx29x20x24fx29x20x24ksx29($8, $a, $b), a2: $11 => $12 => Data_SOP_NP_x2fx3d_Eq_x28x28x28NP_x20x24kx29x20x24fx29x20x24ksx29($8, $11, $12)})), $1, $2);
}

function Data_SOP_POP_popToNP($0, $1) {
 return Data_SOP_NP_mapNP($4 => $5 => $1(undefined)($5), $0);
}

function Data_SOP_NP_x2fx3d_Eq_x28x28x28NP_x20x24kx29x20x24fx29x20x24ksx29($0, $1, $2) {
 switch(Data_SOP_NP_x3dx3d_Eq_x28x28x28NP_x20x24kx29x20x24fx29x20x24ksx29($0, $1, $2)) {
  case 1: return 0;
  case 0: return 1;
 }
}

function Data_SOP_NP_mapNP($0, $1) {
 switch($1.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0(undefined)($1.a1), a2: Data_SOP_NP_mapNP($b => $0(undefined), $1.a2)};
 }
}

function Data_SOP_NP_hd($0) {
 return $0.a1;
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

function JS_Any_toFFI_ToFFI_Any_AnyPtr($0) {
 return Builtin_believe_me($0);
}

function JS_Attribute_to($0, $1) {
 return Prelude_Basics_flip($4 => $5 => JS_Attribute_get($4, $5), $0, $1);
}

function JS_Attribute_set($0, $1) {
 switch($0.h) {
  case 0: return $0.a2($1);
  case 1: return $0.a2({a1: $1});
  case 2: return $0.a2({a1: $1});
  case 3: return $0.a2({a1: $1});
 }
}

function JS_Attribute_get($0, $1) {
 const $2 = $1($0);
 switch($2.h) {
  case 0: return $2.a1;
  case 1: return $2.a1;
  case 2: return $2.a1;
  case 3: return $2.a1;
 }
}

function JS_Attribute_fromPrim($0, $1, $2, $3, $4) {
 return {h: 0, a1: JS_Marshall_tryJS(Builtin_snd($0), () => $1, $2($4)), a2: a => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $3($4)(Builtin_fst($0)(a)))};
}

function JS_Attribute_fromNullablePrim($0, $1, $2, $3, $4) {
 return {h: 1, a1: JS_Marshall_tryJS($8 => JS_Nullable_fromFFI_FromFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29(Builtin_snd($0), $8), () => $1, $2($4)), a2: a => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $3($4)(JS_Nullable_toFFI_ToFFI_x28Maybex20x24ax29_x28Nullablex20x24bx29(Builtin_fst($0), a)))};
}

function JS_Attribute_x2ex3d($0, $1) {
 return JS_Attribute_set($0, $1);
}

function JS_Attribute_x21x3e($0, $1, $2) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), $0($2), $a => JS_Attribute_set($1, $a));
}

function Web_Internal_HtmlTypes_toFFI_ToFFI_MouseEventHandler_MouseEventHandler($0) {
 return $0;
}

function Web_Internal_HtmlTypes_toFFI_ToFFI_KeyboardEventHandler_KeyboardEventHandler($0) {
 return $0;
}

function Web_Internal_HtmlTypes_toFFI_ToFFI_InputEventHandler_InputEventHandler($0) {
 return $0;
}

function Web_Internal_HtmlTypes_toFFI_ToFFI_EventHandlerNonNull_EventHandlerNonNull($0) {
 return $0;
}

function Web_Internal_HtmlTypes_safeCast_SafeCast_HTMLInputElement($0) {
 return JS_Inheritance_unsafeCastOnPrototypeName('HTMLInputElement', $0);
}

function Web_Internal_HtmlTypes_safeCast_SafeCast_HTMLElement($0) {
 return JS_Inheritance_unsafeCastOnPrototypeName('HTMLElement', $0);
}

function Web_Internal_HtmlTypes_fromFFI_FromFFI_MouseEventHandler_MouseEventHandler($0) {
 return {a1: $0};
}

function Web_Internal_HtmlTypes_fromFFI_FromFFI_KeyboardEventHandler_KeyboardEventHandler($0) {
 return {a1: $0};
}

function Web_Internal_HtmlTypes_fromFFI_FromFFI_InputEventHandler_InputEventHandler($0) {
 return {a1: $0};
}

function Web_Internal_HtmlTypes_fromFFI_FromFFI_EventHandlerNonNull_EventHandlerNonNull($0) {
 return {a1: $0};
}

function Web_Internal_DomTypes_safeCast_SafeCast_Element($0) {
 return JS_Inheritance_unsafeCastOnPrototypeName('Element', $0);
}

function Web_Internal_DomTypes_fromFFI_FromFFI_Element_Element($0) {
 return {a1: $0};
}

function Text_Html_Node_n__10970_2788_esc($0) {
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
  case 0: return Prelude_Types_String_x2bx2b('<', Prelude_Types_String_x2bx2b($0.a1, Prelude_Types_String_x2bx2b(Text_Html_Node_attrs($0.a3), Prelude_Types_String_x2bx2b('>', Prelude_Types_String_x2bx2b(Text_Html_Node_n__11072_2894_go($0, {h: 0}, $0.a4), Prelude_Types_String_x2bx2b('</', Prelude_Types_String_x2bx2b($0.a1, '>')))))));
 }
}

function Text_Html_Node_escape($0) {
 return Prelude_Types_fastConcat(Prelude_Types_map_Functor_List($5 => Text_Html_Node_n__10970_2788_esc($5), Prelude_Types_fastUnpack($0)));
}

function Text_Html_Node_attrs($0) {
 const $1 = Text_Html_Attribute_displayAttributes($0);
 switch(Data_String_null($1)) {
  case 1: return '';
  case 0: return Prelude_Types_String_x2bx2b(' ', $1);
 }
}

function Text_Html_Attribute_onInput($0) {
 return {h: 3, a1: {h: 12, a1: $3 => ({a1: $0($3)})}};
}

function Text_Html_Attribute_onEnterDown($0) {
 return {h: 3, a1: {h: 9, a1: k => Data_Maybe_toMaybe(Prelude_EqOrd_x3dx3d_Eq_String(k.a2, 'Enter'), () => $0)}};
}

function Text_Html_Attribute_onChange($0) {
 return {h: 3, a1: {h: 12, a1: $3 => ({a1: $0($3)})}};
}

function Text_Html_Attribute_getEvents($0) {
 return Text_Html_Attribute_n__3149_1133_go({h: 0}, $0);
}

function Text_Html_Attribute_displayAttributes($0) {
 return Prelude_Types_fastConcat(Data_List_intersperse(' ', Data_List_mapMaybe($8 => Text_Html_Attribute_displayAttribute($8), $0)));
}

function Text_Html_Attribute_displayAttribute($0) {
 switch($0.h) {
  case 0: return {a1: Prelude_Types_String_x2bx2b('id=\"', Prelude_Types_String_x2bx2b($0.a1, '\"'))};
  case 1: return {a1: Prelude_Types_String_x2bx2b($0.a1, Prelude_Types_String_x2bx2b('=\"', Prelude_Types_String_x2bx2b($0.a2, '\"')))};
  case 2: {
   switch($0.a2) {
    case 1: return {a1: $0.a1};
    case 0: return {h: 0};
   }
  }
  case 3: return {h: 0};
 }
}

function Text_Html_Attribute_dispAttr($0, $1, $2) {
 return {h: 1, a1: $0, a2: $1($2)};
}

function Rhone_JS_Source_getValue_HasValue_HTMLInputElement($0) {
 return JS_Attribute_to($3 => Web_Raw_Html_HTMLInputElement_value($3), $0);
}

function Rhone_JS_Source_value($0, $1) {
 return {h: 3, a1: r => $0.a2(undefined)(Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Dom_Interface_getElementByRef($1.a1, r), $11 => $1.a2($11)))};
}

function Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($0) {
 return Web_Raw_Html_MouseEventHandler_toMouseEventHandler($3 => $4 => JS_Util_runJS($0($3), $4));
}

function Web_Html_callback_Callback_KeyboardEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20KeyboardEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($0) {
 return Web_Raw_Html_KeyboardEventHandler_toKeyboardEventHandler($3 => $4 => JS_Util_runJS($0($3), $4));
}

function Web_Html_callback_Callback_InputEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20InputEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($0) {
 return Web_Raw_Html_InputEventHandler_toInputEventHandler($3 => $4 => JS_Util_runJS($0($3), $4));
}

function Web_Html_callback_Callback_EventHandlerNonNull_x28x25pix20RigWx20Explicitx20Nothingx20Eventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($0) {
 return Web_Raw_Html_EventHandlerNonNull_toEventHandlerNonNull($3 => $4 => Prelude_IO_map_Functor_IO($7 => JS_Any_toFFI_ToFFI_Any_AnyPtr($7), $b => JS_Util_runJS($0($3), $b), $4));
}

function Web_Raw_Html_HTMLInputElement_value($0) {
 return JS_Attribute_fromPrim(csegen_43(), 'HTMLInputElement.getvalue', $6 => $7 => Web_Internal_HtmlPrim_HTMLInputElement_prim__value($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_HTMLInputElement_prim__setValue($c, $d, $e), $0);
}

function Web_Raw_Html_MouseEventHandler_toMouseEventHandler($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_HtmlPrim_MouseEventHandler_prim__toMouseEventHandler($0, $5));
}

function Web_Raw_Html_KeyboardEventHandler_toKeyboardEventHandler($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_HtmlPrim_KeyboardEventHandler_prim__toKeyboardEventHandler($0, $5));
}

function Web_Raw_Html_InputEventHandler_toInputEventHandler($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_HtmlPrim_InputEventHandler_prim__toInputEventHandler($0, $5));
}

function Web_Raw_Html_EventHandlerNonNull_toEventHandlerNonNull($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_HtmlPrim_EventHandlerNonNull_prim__toEventHandlerNonNull($0, $5));
}

function Web_Raw_Html_HTMLInputElement_setCustomValidity($0, $1) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $6 => Web_Internal_HtmlPrim_HTMLInputElement_prim__setCustomValidity($0, $1, $6));
}

function Web_Raw_Html_GlobalEventHandlers_onmouseup($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmouseup', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseup($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseup($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onmouseover($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmouseover', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseover($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseover($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onmouseout($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmouseout', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseout($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseout($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onmousemove($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmousemove', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmousemove($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmousemove($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onmouseleave($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmouseleave', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseleave($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseleave($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onmouseenter($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmouseenter', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmouseenter($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmouseenter($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onmousedown($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonmousedown', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onmousedown($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnmousedown($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onkeyup($0) {
 return JS_Attribute_fromNullablePrim(csegen_72(), 'GlobalEventHandlers.getonkeyup', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onkeyup($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnkeyup($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onkeydown($0) {
 return JS_Attribute_fromNullablePrim(csegen_72(), 'GlobalEventHandlers.getonkeydown', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onkeydown($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnkeydown($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_oninput($0) {
 return JS_Attribute_fromNullablePrim({a1: $4 => Web_Internal_HtmlTypes_toFFI_ToFFI_InputEventHandler_InputEventHandler($4), a2: $8 => Web_Internal_HtmlTypes_fromFFI_FromFFI_InputEventHandler_InputEventHandler($8)}, 'GlobalEventHandlers.getoninput', $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__oninput($d, $e), $13 => $14 => $15 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOninput($13, $14, $15), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_ondblclick($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getondblclick', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__ondblclick($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOndblclick($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onclick($0) {
 return JS_Attribute_fromNullablePrim(csegen_57(), 'GlobalEventHandlers.getonclick', $6 => $7 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onclick($6, $7), $c => $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnclick($c, $d, $e), Builtin_believe_me($0));
}

function Web_Raw_Html_GlobalEventHandlers_onchange($0) {
 return JS_Attribute_fromNullablePrim({a1: $4 => Web_Internal_HtmlTypes_toFFI_ToFFI_EventHandlerNonNull_EventHandlerNonNull($4), a2: $8 => Web_Internal_HtmlTypes_fromFFI_FromFFI_EventHandlerNonNull_EventHandlerNonNull($8)}, 'GlobalEventHandlers.getonchange', $d => $e => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__onchange($d, $e), $13 => $14 => $15 => Web_Internal_HtmlPrim_GlobalEventHandlers_prim__setOnchange($13, $14, $15), Builtin_believe_me($0));
}

function Data_MSF_Util_n__5209_7278_next($0, $1, $2) {
 switch($2.h) {
  case 0: return {a1: $1, a2: {a1: $0, a2: {h: 0}}};
  case undefined: return {a1: $2.a2, a2: {a1: $2.a1, a2: {h: 0}}};
 }
}

function Data_MSF_Util_n__4926_6989_g($0, $1, $2) {
 const $6 = $1($2.a2.a1)($2.a1);
 return {a1: $6, a2: {a1: $6, a2: {h: 0}}};
}

function Data_MSF_Util_when_($0) {
 return {h: 2, a1: vi => Data_MSF_Event_toEvent($0(vi), () => 0)};
}

function Data_MSF_Util_unfold($0, $1) {
 return {h: 9, a1: $1, a2: {h: 2, a1: $5 => $0(Data_SOP_NP_hd($5))}};
}

const Data_MSF_Util_rightOnEvent = __lazy(function () {
 const $0 = $1 => {
  switch($1.h) {
   case undefined: {
    switch($1.a1.h) {
     case 1: {
      switch($1.a2.h) {
       case undefined: {
        switch($1.a2.a2.h) {
         case 0: return Prelude_Interfaces_x24x3e($8 => $9 => $a => $b => Data_MSF_Event_map_Functor_Event($a, $b), $1.a2.a1, $1.a1.a1);
         default: return {h: 0};
        }
       }
       default: return {h: 0};
      }
     }
     default: return {h: 0};
    }
   }
   default: return {h: 0};
  }
 };
 return {h: 2, a1: $0};
});

function Data_MSF_Util_once($0) {
 return {h: 12, a1: {h: 1, a1: {h: 0, a1: {a1: 0, a2: {a1: $0}}}}, a2: () => $8 => Data_MSF_Util_never()};
}

function Data_MSF_Util_observeWith($0) {
 return {h: 4, a1: {h: 6, a1: {a1: {h: 0}, a2: {a1: $0, a2: {h: 0}}}}, a2: Data_MSF_Util_hd()};
}

const Data_MSF_Util_never = __lazy(function () {
 return {h: 1, a1: {h: 0}};
});

function Data_MSF_Util_is($0, $1) {
 return Data_MSF_Util_when_($4 => $0.a1($1)($4));
}

function Data_MSF_Util_ifTrue($0, $1, $2) {
 return {h: 4, a1: Data_MSF_Util_bool($1), a2: {h: 8, a1: {a1: $2, a2: {a1: {h: 1, a1: $0.a2}, a2: {h: 0}}}}};
}

function Data_MSF_Util_ifIs($0, $1, $2, $3) {
 return Data_MSF_Util_ifTrue($1, $7 => $0.a1($2)($7), $3);
}

function Data_MSF_Util_ifFalse($0, $1, $2) {
 return {h: 4, a1: Data_MSF_Util_bool($1), a2: {h: 8, a1: {a1: {h: 1, a1: $0.a2}, a2: {a1: $2, a2: {h: 0}}}}};
}

function Data_MSF_Util_ifEvent($0, $1) {
 return {h: 4, a1: Data_MSF_Util_event(), a2: {h: 8, a1: {a1: $1, a2: {a1: {h: 1, a1: $0.a2}, a2: {h: 0}}}}};
}

function Data_MSF_Util_hold($0) {
 return Data_MSF_Util_accumulateWith(ev => v => Data_MSF_Event_fromEvent(() => v, ev), $0);
}

const Data_MSF_Util_hd = __lazy(function () {
 return {h: 2, a1: $1 => Data_SOP_NP_hd($1)};
});

function Data_MSF_Util_firstArg($0, $1) {
 return {h: 4, a1: {h: 6, a1: {a1: {h: 1, a1: $1}, a2: {a1: {h: 0}, a2: {h: 0}}}}, a2: $0};
}

const Data_MSF_Util_event = __lazy(function () {
 return {h: 2, a1: $1 => Data_MSF_Event_event(() => ({h: 1, a1: {h: 0, a1: 0}}), () => $7 => ({h: 0, a1: $7}), $1)};
});

function Data_MSF_Util_cycle($0) {
 return Data_MSF_Util_unfold($4 => Data_MSF_Util_n__5209_7278_next($0.a1, $0.a2, $4), {a1: $0.a1, a2: $0.a2});
}

function Data_MSF_Util_bool($0) {
 const $1 = vi => {
  switch($0(vi)) {
   case 1: return {h: 0, a1: vi};
   case 0: return {h: 1, a1: {h: 0, a1: vi}};
  }
 };
 return {h: 2, a1: $1};
}

function Data_MSF_Util_accumulateWith($0, $1) {
 return {h: 9, a1: $1, a2: {h: 2, a1: $5 => Data_MSF_Util_n__4926_6989_g($1, $0, $5)}};
}

function Data_MSF_Util_x3cx7cx3e($0, $1) {
 return {h: 4, a1: {h: 6, a1: {a1: $0, a2: {a1: $1, a2: {h: 0}}}}, a2: {h: 2, a1: $a => Data_MSF_Event_unionL($a.a1, $a.a2.a1)}};
}

function Data_MSF_Event_map_Functor_Event($0, $1) {
 switch($1.h) {
  case 0: return {h: 0};
  case undefined: return {a1: $0($1.a1)};
 }
}

function Data_MSF_Event_unionL($0, $1) {
 switch($0.h) {
  case undefined: return {a1: $0.a1};
  default: return $1;
 }
}

function Data_MSF_Event_toEvent($0, $1) {
 switch($0) {
  case 0: return {h: 0};
  case 1: return {a1: $1()};
 }
}

function Data_MSF_Event_fromEvent($0, $1) {
 switch($1.h) {
  case 0: return $0();
  case undefined: return $1.a1;
 }
}

function Data_MSF_Event_event($0, $1, $2) {
 switch($2.h) {
  case 0: return $0();
  case undefined: return $1()($2.a1);
 }
}

function Data_MSF_Running_stepPar($0, $1, $2) {
 switch($1.h) {
  case 0: return $0.a1.a2(undefined)({a1: {h: 0}, a2: {h: 0}});
  case undefined: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2.a1))($1b => $0.a2(undefined)(undefined)(Data_MSF_Running_stepPar($0, $1.a2, $2.a2))($2a => $0.a1.a2(undefined)({a1: {a1: $1b.a1, a2: $2a.a1}, a2: {a1: $1b.a2, a2: $2a.a2}})));
 }
}

function Data_MSF_Running_stepFan($0, $1, $2) {
 switch($1.h) {
  case 0: return $0.a1.a2(undefined)({a1: {h: 0}, a2: {h: 0}});
  case undefined: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2))($19 => $0.a2(undefined)(undefined)(Data_MSF_Running_stepFan($0, $1.a2, $2))($28 => $0.a1.a2(undefined)({a1: {a1: $19.a1, a2: $28.a1}, a2: {a1: $19.a2, a2: $28.a2}})));
 }
}

function Data_MSF_Running_stepCollect($0, $1, $2) {
 switch($2.h) {
  case 0: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2.a1))($12 => $0.a1.a2(undefined)({a1: $12.a1, a2: {a1: $12.a2, a2: $1.a2}}));
  case 1: return $0.a2(undefined)(undefined)(Data_MSF_Running_stepCollect($0, $1.a2, $2.a1))($2b => $0.a1.a2(undefined)({a1: $2b.a1, a2: {a1: $1.a1, a2: $2b.a2}}));
 }
}

function Data_MSF_Running_stepChoice($0, $1, $2) {
 switch($2.h) {
  case 0: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2.a1))($12 => $0.a1.a2(undefined)({a1: {h: 0, a1: $12.a1}, a2: {a1: $12.a2, a2: $1.a2}}));
  case 1: return $0.a2(undefined)(undefined)(Data_MSF_Running_stepChoice($0, $1.a2, $2.a1))($2c => $0.a1.a2(undefined)({a1: {h: 1, a1: $2c.a1}, a2: {a1: $1.a1, a2: $2c.a2}}));
 }
}

function Data_MSF_Running_step($0, $1, $2) {
 switch($1.h) {
  case 1: return $0.a1.a2(undefined)({a1: $1.a1, a2: $1});
  case 0: return $0.a1.a2(undefined)({a1: $2, a2: {h: 0}});
  case 2: return $0.a1.a2(undefined)({a1: $1.a1($2), a2: $1});
  case 3: return Prelude_Interfaces_x3cx24x3e($0.a1.a1, $23 => ({a1: $23, a2: $1}), $1.a1($2));
  case 4: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2))($36 => $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a2, $36.a1))($45 => $0.a1.a2(undefined)({a1: $45.a1, a2: {h: 4, a1: $36.a2, a2: $45.a2}})));
  case 5: return Prelude_Interfaces_x3cx24x3e($0.a1.a1, $56 => ({a1: $56.a1, a2: {h: 5, a1: $56.a2}}), Data_MSF_Running_stepPar($0, $1.a1, $2));
  case 6: return Prelude_Interfaces_x3cx24x3e($0.a1.a1, $65 => ({a1: $65.a1, a2: {h: 6, a1: $65.a2}}), Data_MSF_Running_stepFan($0, $1.a1, $2));
  case 7: return Prelude_Interfaces_x3cx24x3e($0.a1.a1, $74 => ({a1: $74.a1, a2: {h: 7, a1: $74.a2}}), Data_MSF_Running_stepChoice($0, $1.a1, $2));
  case 8: return Prelude_Interfaces_x3cx24x3e($0.a1.a1, $83 => ({a1: $83.a1, a2: {h: 8, a1: $83.a2}}), Data_MSF_Running_stepCollect($0, $1.a1, $2));
  case 9: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a2, {a1: $1.a1, a2: {a1: $2, a2: {h: 0}}}))($9e => $0.a1.a2(undefined)({a1: $9e.a1.a2.a1, a2: {h: 9, a1: $9e.a1.a1, a2: $9e.a2}}));
  case 10: return $0.a2(undefined)(undefined)($1.a2(undefined)($ba => Data_MSF_Running_step($1.a1, $1.a3, $ba))($2))($c1 => $0.a1.a2(undefined)({a1: $c1.a1, a2: {h: 10, a1: $1.a1, a2: $cd => $1.a2(undefined), a3: $c1.a2}}));
  case 11: {
   const $dd = $de => {
    switch($de.a1.h) {
     case 0: return Data_MSF_Running_step($0, $1.a2($de.a1.a1), $2);
     case 1: return $0.a1.a2(undefined)({a1: $de.a1.a1, a2: {h: 11, a1: $de.a2, a2: $1.a2}});
    }
   };
   return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2))($dd);
  }
  case 12: {
   const $fd = $fe => {
    switch($fe.a1.h) {
     case 0: return $0.a1.a2(undefined)({a1: $fe.a1.a1.a2, a2: $1.a2()($fe.a1.a1.a1)});
     case 1: return $0.a1.a2(undefined)({a1: $fe.a1.a1, a2: {h: 12, a1: $fe.a2, a2: () => $1.a2()}});
    }
   };
   return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2))($fd);
  }
  case 13: return $0.a2(undefined)(undefined)(Data_MSF_Running_step($0, $1.a1, $2))($125 => $0.a1.a2(undefined)({a1: {a1: $125.a2, a2: {a1: $125.a1, a2: {h: 0}}}, a2: {h: 13, a1: $125.a2}}));
 }
}

function Control_Monad_Dom_Interface_case__prepareNode_2489($0, $1, $2, $3, $4, $5, $6) {
 switch($6.h) {
  case 0: return $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Interface_prepareNodes($0, $1, $3))($16 => $0.a1.a1.a2(undefined)({a1: {h: 0, a1: $2, a2: $5, a3: $4, a4: $16.a1}, a2: $16.a2}));
  default: return $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Interface_getRef($2, $0, $4, $5))($34 => $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Interface_prepareNodes($0, $1, $3))($44 => $0.a1.a1.a2(undefined)({a1: {h: 0, a1: $2, a2: $5, a3: $34.a1, a4: $44.a1}, a2: Prelude_Types_List_tailRecAppend(Prelude_Types_map_Functor_List($57 => $0.a3(undefined)($34.a2)($57), $6), $44.a2)})));
 }
}

function Control_Monad_Dom_Interface_n__14269_2335_go($0, $1, $2, $3, $4) {
 switch($3.h) {
  case 0: return $0.a1.a1.a2(undefined)({h: 1, a1: {a1: Prelude_Types_List_reverse($4.a1), a2: $4.a2}});
  case undefined: return $0.a1.a2(undefined)(undefined)(Control_Monad_Dom_Interface_prepareNode($0, $1, $3.a1))($22 => $0.a1.a1.a2(undefined)({h: 0, a1: $3.a2, a2: {a1: {a1: $22.a1, a2: $4.a1}, a2: Prelude_Types_List_tailRecAppend($22.a2, $4.a2)}}));
 }
}

function Control_Monad_Dom_Interface_strictGetElementById($0, $1, $2) {
 const $a = $b => {
  switch($b.h) {
   case 0: return Control_Monad_Error_Interface_throwError_MonadError_x24e_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), {h: 0, a1: Prelude_Types_String_x2bx2b('Control.Monad.Dom.Interface.strictGetElementById: Could not find ', Prelude_Types_String_x2bx2b($1, Prelude_Types_String_x2bx2b(' with id ', $2)))});
   case undefined: return Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $b.a1);
  }
 };
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Web_Dom_castElementById($0, $2), $a);
}

function Control_Monad_Dom_Interface_rawInnerHtmlAt($0, $1, $2) {
 return $0.a2(undefined)(Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Dom_Interface_strictGetElementById(csegen_98(), $1.a1, $1.a3), elem => JS_Attribute_x2ex3d(Web_Raw_Dom_InnerHTML_innerHTML(elem), $2)));
}

function Control_Monad_Dom_Interface_prepareNodes($0, $1, $2) {
 return $1.a2(undefined)(undefined)(undefined)(undefined)($11 => $12 => Control_Monad_Dom_Interface_n__14269_2335_go($0, $1, $2, $11, $12))($2)({a1: {h: 0}, a2: {h: 0}})(undefined);
}

function Control_Monad_Dom_Interface_prepareNode($0, $1, $2) {
 switch($2.h) {
  case 0: return Control_Monad_Dom_Interface_case__prepareNode_2489($0, $1, $2.a1, $2.a4, $2.a3, $2.a2, Text_Html_Attribute_getEvents($2.a3));
  case 1: return $0.a1.a1.a2(undefined)({a1: $2, a2: {h: 0}});
  case 2: return $0.a1.a1.a2(undefined)({a1: $2, a2: {h: 0}});
 }
}

function Control_Monad_Dom_Interface_innerHtmlAt($0, $1, $2, $3, $4) {
 return $2.a1.a2(undefined)(undefined)($0.a2(undefined)(Control_Monad_Dom_Interface_strictGetElementById(csegen_98(), $3.a1, $3.a3)))(elem => $2.a1.a2(undefined)(undefined)(Control_Monad_Dom_Interface_prepareNode($2, $1, $4))($28 => Prelude_Interfaces_x3ex3e($2.a1, $0.a2(undefined)(JS_Attribute_x2ex3d(Web_Raw_Dom_InnerHTML_innerHTML(elem), Text_Html_Node_render($28.a1))), () => Data_Iterable_forM_($3d => $3e => $3f => $40 => $41 => $42 => $43 => $44 => Data_Iterable_iterM_Iterable_x28Listx20x24ax29_x24a($40, $41, $42, $43, $44), $1, x => x, $28.a2))));
}

function Control_Monad_Dom_Interface_getRef($0, $1, $2, $3) {
 const $4 = Text_Html_Attribute_getId($2);
 switch($4.h) {
  case undefined: return $1.a1.a1.a2(undefined)({a1: $2, a2: {a1: $0, a2: $3, a3: $4.a1}});
  case 0: return Prelude_Interfaces_x3cx24x3e($1.a1.a1.a1, i => ({a1: {a1: {h: 0, a1: i}, a2: $2}, a2: {a1: $0, a2: $3, a3: i}}), $1.a2);
 }
}

function Control_Monad_Dom_Interface_getElementByRef($0, $1) {
 return Control_Monad_Dom_Interface_strictGetElementById($0, $1.a1, $1.a3);
}

function Data_Iterable_n__3437_1437_go($0, $1, $2, $3, $4, $5, $6) {
 switch($5.h) {
  case 0: return $0.a1.a1.a2(undefined)({h: 1, a1: $3($6)});
  case undefined: return Prelude_Interfaces_x3cx24x3e($0.a1.a1.a1, $18 => ({h: 0, a1: $5.a2, a2: $18}), $4($5.a1)($6));
 }
}

function Data_Iterable_iterM_Iterable_x28Listx20x24ax29_x24a($0, $1, $2, $3, $4) {
 return $0.a2(undefined)(undefined)(undefined)(undefined)($13 => $14 => Data_Iterable_n__3437_1437_go($0, $4, $3, $2, $1, $13, $14))($4)($3)(undefined);
}

function Data_Iterable_forM_($0, $1, $2, $3) {
 return $0(undefined)(undefined)(undefined)($1)(e => $11 => $2(e))($15 => 0)(0)($3);
}

function Control_MonadRec_n__5714_2558_conv($0, $1, $2, $3, $4) {
 switch($4.h) {
  case 0: return {h: 1, a1: {h: 0, a1: $4.a1}};
  case 1: {
   switch($4.a1.h) {
    case 1: return {h: 1, a1: {h: 1, a1: $4.a1.a1}};
    case 0: return {h: 0, a1: $4.a1.a1, a2: $4.a1.a2};
   }
  }
 }
}

function Control_MonadRec_tailRecM_MonadRec_x28x28EitherTx20x24ex29x20x24mx29($0, $1, $2, $3) {
 return $0.a2(undefined)(undefined)(undefined)(undefined)($12 => $13 => Control_MonadRec_convE($0.a1.a1.a1, $1, $12, $13))($2)($3)(undefined);
}

function Control_MonadRec_trIO($0, $1, $2, $3) {
 return Control_MonadRec_n__5335_2172_run($1, $2, $0, $1, $2, $3);
}

function Control_MonadRec_convE($0, $1, $2, $3) {
 return $0(undefined)(undefined)($b => Control_MonadRec_n__5714_2558_conv($0, $3, $2, $1, $b))($1($2)($3));
}

function Control_Monad_Dom_Event_mouseInfo($0) {
 return Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $2d => $2e => $2f => $30 => $31 => $32 => $33 => $34 => $35 => $36 => ({a1: $2d, a2: $2e, a3: $2f, a4: $30, a5: $31, a6: $32, a7: $33, a8: $34, a9: $35, a10: $36})), Web_Raw_UIEvents_MouseEvent_button($0)), Web_Raw_UIEvents_MouseEvent_buttons($0)), Web_Raw_UIEvents_MouseEvent_clientX($0)), Web_Raw_UIEvents_MouseEvent_clientY($0)), Web_Raw_UIEvents_MouseEvent_screenX($0)), Web_Raw_UIEvents_MouseEvent_screenY($0)), Web_Raw_UIEvents_MouseEvent_altKey($0)), Web_Raw_UIEvents_MouseEvent_ctrlKey($0)), Web_Raw_UIEvents_MouseEvent_metaKey($0)), Web_Raw_UIEvents_MouseEvent_shiftKey($0));
}

function Control_Monad_Dom_Event_keyInfo($0) {
 return Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_x3cx2ax3e_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $25 => $26 => $27 => $28 => $29 => $2a => $2b => $2c => ({a1: $25, a2: $26, a3: $27, a4: $28, a5: $29, a6: $2a, a7: $2b, a8: $2c})), Web_Raw_UIEvents_KeyboardEvent_key($0)), Web_Raw_UIEvents_KeyboardEvent_code($0)), Web_Raw_UIEvents_KeyboardEvent_location($0)), Web_Raw_UIEvents_KeyboardEvent_isComposing($0)), Web_Raw_UIEvents_KeyboardEvent_altKey($0)), Web_Raw_UIEvents_KeyboardEvent_ctrlKey($0)), Web_Raw_UIEvents_KeyboardEvent_metaKey($0)), Web_Raw_UIEvents_KeyboardEvent_shiftKey($0));
}

function Control_Monad_Dom_Event_inputInfo($0) {
 return Prelude_Interfaces_x3cx24x3e(csegen_109(), $5 => $5, Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $b => Control_Monad_Dom_Event_prim__input(Builtin_believe_me($0), $b)));
}

function Control_Monad_Dom_Event_changeInfo($0) {
 return Prelude_Interfaces_x3cx24x3e(csegen_109(), $5 => $5, Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $b => Control_Monad_Dom_Event_prim__input($0, $b)));
}

function Web_Raw_UIEvents_MouseEvent_shiftKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'MouseEvent.shiftKey', $8 => Web_Internal_UIEventsPrim_MouseEvent_prim__shiftKey(Builtin_believe_me($0), $8));
}

function Web_Raw_UIEvents_KeyboardEvent_shiftKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'KeyboardEvent.shiftKey', $8 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__shiftKey($0, $8));
}

function Web_Raw_UIEvents_MouseEvent_screenY($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_MouseEvent_prim__screenY(Builtin_believe_me($0), $5));
}

function Web_Raw_UIEvents_MouseEvent_screenX($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_MouseEvent_prim__screenX(Builtin_believe_me($0), $5));
}

function Web_Raw_UIEvents_MouseEvent_metaKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'MouseEvent.metaKey', $8 => Web_Internal_UIEventsPrim_MouseEvent_prim__metaKey(Builtin_believe_me($0), $8));
}

function Web_Raw_UIEvents_KeyboardEvent_metaKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'KeyboardEvent.metaKey', $8 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__metaKey($0, $8));
}

function Web_Raw_UIEvents_KeyboardEvent_location($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__location($0, $5));
}

function Web_Raw_UIEvents_KeyboardEvent_key($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__key($0, $5));
}

function Web_Raw_UIEvents_KeyboardEvent_isComposing($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'KeyboardEvent.isComposing', $8 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__isComposing($0, $8));
}

function Web_Raw_UIEvents_MouseEvent_ctrlKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'MouseEvent.ctrlKey', $8 => Web_Internal_UIEventsPrim_MouseEvent_prim__ctrlKey(Builtin_believe_me($0), $8));
}

function Web_Raw_UIEvents_KeyboardEvent_ctrlKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'KeyboardEvent.ctrlKey', $8 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__ctrlKey($0, $8));
}

function Web_Raw_UIEvents_KeyboardEvent_code($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__code($0, $5));
}

function Web_Raw_UIEvents_MouseEvent_clientY($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_MouseEvent_prim__clientY(Builtin_believe_me($0), $5));
}

function Web_Raw_UIEvents_MouseEvent_clientX($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_MouseEvent_prim__clientX(Builtin_believe_me($0), $5));
}

function Web_Raw_UIEvents_MouseEvent_buttons($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_MouseEvent_prim__buttons(Builtin_believe_me($0), $5));
}

function Web_Raw_UIEvents_MouseEvent_button($0) {
 return Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $5 => Web_Internal_UIEventsPrim_MouseEvent_prim__button(Builtin_believe_me($0), $5));
}

function Web_Raw_UIEvents_MouseEvent_altKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'MouseEvent.altKey', $8 => Web_Internal_UIEventsPrim_MouseEvent_prim__altKey(Builtin_believe_me($0), $8));
}

function Web_Raw_UIEvents_KeyboardEvent_altKey($0) {
 return JS_Marshall_tryJS($3 => JS_Boolean_fromFFI_FromFFI_Bool_Boolean($3), () => 'KeyboardEvent.altKey', $8 => Web_Internal_UIEventsPrim_KeyboardEvent_prim__altKey($0, $8));
}

function Control_Monad_Dom_DomIO_n__20132_28069_handle($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $13 => ($9.value)), sf1 => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Data_MSF_Running_step(csegen_130(), sf1, $a)($5), $24 => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $2a => ($9.value=$24.a2))));
}

function Control_Monad_Dom_DomIO_n__18220_26032_handle($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), $8($a), $12 => Prelude_Types_maybe(() => csegen_131(), () => $6, $9($12)));
}

function Control_Monad_Dom_DomIO_tailRecM_MonadRec_x28x28DomIOx20x24ex29x20x24iox29($0, $1, $2, $3, $4) {
 return $0.a2(undefined)(undefined)(undefined)(undefined)($13 => $14 => Control_Monad_Dom_DomIO_convR($1, $4, $13, $14))($2)($3)(undefined);
}

function Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2) {
 return $0.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_map_Functor_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2, $3) {
 return $0(undefined)(undefined)($1)($2($3));
}

function Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2) {
 return $0.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_liftIO_HasIO_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2) {
 return $0.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_join_Monad_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2) {
 return Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $7 => $7, $2);
}

function Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2, $3) {
 return $0.a2(undefined)(undefined)($1($3))($f => $2($f)($3));
}

function Control_Monad_Dom_DomIO_x3cx2ax3e_Applicative_x28x28DomIOx20x24evx29x20x24iox29($0, $1, $2, $3) {
 return $0.a3(undefined)(undefined)($1($3))($2($3));
}

function Control_Monad_Dom_DomIO_registerImpl($0, $1, $2) {
 const $e = t => {
  const $f = {a1: $0.a1, a2: $0.a2, a3: $0.a3};
  switch($1.h) {
   case 12: return JS_Attribute_x21x3e($16 => Web_Html_callback_Callback_InputEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20InputEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($16), Web_Raw_Html_GlobalEventHandlers_oninput(t), $1d => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $28 => Control_Monad_Dom_Event_inputInfo($28), $1.a1, $1d));
   case 11: return JS_Attribute_x21x3e($2f => Web_Html_callback_Callback_EventHandlerNonNull_x28x25pix20RigWx20Explicitx20Nothingx20Eventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($2f), Web_Raw_Html_GlobalEventHandlers_onchange(t), $36 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $41 => Control_Monad_Dom_Event_changeInfo($41), $1.a1, $36));
   case 0: return JS_Attribute_x21x3e($48 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($48), Web_Raw_Html_GlobalEventHandlers_onclick(t), $4f => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $5a => Control_Monad_Dom_Event_mouseInfo($5a), $1.a1, $4f));
   case 1: return JS_Attribute_x21x3e($61 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($61), Web_Raw_Html_GlobalEventHandlers_ondblclick(t), $68 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $73 => Control_Monad_Dom_Event_mouseInfo($73), $1.a1, $68));
   case 9: return JS_Attribute_x21x3e($7a => Web_Html_callback_Callback_KeyboardEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20KeyboardEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($7a), Web_Raw_Html_GlobalEventHandlers_onkeydown(t), $81 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $8c => Control_Monad_Dom_Event_keyInfo($8c), $1.a1, $81));
   case 10: return JS_Attribute_x21x3e($93 => Web_Html_callback_Callback_KeyboardEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20KeyboardEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($93), Web_Raw_Html_GlobalEventHandlers_onkeyup(t), $9a => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $a5 => Control_Monad_Dom_Event_keyInfo($a5), $1.a1, $9a));
   case 2: return JS_Attribute_x21x3e($ac => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($ac), Web_Raw_Html_GlobalEventHandlers_onmousedown(t), $b3 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $be => Control_Monad_Dom_Event_mouseInfo($be), $1.a1, $b3));
   case 3: return JS_Attribute_x21x3e($c5 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($c5), Web_Raw_Html_GlobalEventHandlers_onmouseup(t), $cc => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $d7 => Control_Monad_Dom_Event_mouseInfo($d7), $1.a1, $cc));
   case 4: return JS_Attribute_x21x3e($de => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($de), Web_Raw_Html_GlobalEventHandlers_onmouseenter(t), $e5 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $f0 => Control_Monad_Dom_Event_mouseInfo($f0), $1.a1, $e5));
   case 5: return JS_Attribute_x21x3e($f7 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($f7), Web_Raw_Html_GlobalEventHandlers_onmouseleave(t), $fe => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $109 => Control_Monad_Dom_Event_mouseInfo($109), $1.a1, $fe));
   case 6: return JS_Attribute_x21x3e($110 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($110), Web_Raw_Html_GlobalEventHandlers_onmouseover(t), $117 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $122 => Control_Monad_Dom_Event_mouseInfo($122), $1.a1, $117));
   case 7: return JS_Attribute_x21x3e($129 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($129), Web_Raw_Html_GlobalEventHandlers_onmouseout(t), $130 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $13b => Control_Monad_Dom_Event_mouseInfo($13b), $1.a1, $130));
   case 8: return JS_Attribute_x21x3e($142 => Web_Html_callback_Callback_MouseEventHandler_x28x25pix20RigWx20Explicitx20Nothingx20MouseEventx20x28JSIOx20x28x7cUnitx2cMkUnitx7cx29x29x29($142), Web_Raw_Html_GlobalEventHandlers_onmousemove(t), $149 => Control_Monad_Dom_DomIO_n__18220_26032_handle($2.a1, $2.a2, $0.a1, $0.a2, $0.a3, $f, $2.a3, $1, $154 => Control_Monad_Dom_Event_mouseInfo($154), $1.a1, $149));
  }
 };
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Dom_Interface_strictGetElementById(csegen_98(), $0.a1, $0.a3), $e);
}

function Control_Monad_Dom_DomIO_reactimateDom_($0, $1, $2, $3) {
 const $c = hRef => {
  const $10 = ev => {
   const $1d = $1e => {
    switch($1e.h) {
     case undefined: return $1e.a1(ev);
     case 0: return csegen_131();
    }
   };
   return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $19 => (hRef.value)), $1d);
  };
  const $d = {a1: $1, a2: $3, a3: $10};
  return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), $2($d), $2a => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Data_IORef_newIORef(csegen_37(), $2a.a1), sfRef => Prelude_Interfaces_x3ex3e(csegen_124(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $3d => (hRef.value={a1: $42 => Control_Monad_Dom_DomIO_n__20132_28069_handle($3, $2, $1, $0, hRef, $d, $2a.a1, $2a.a2, $2a, sfRef, $42)})), () => Prelude_Interfaces_x3ex3e(csegen_124(), Prelude_Interfaces_traverse_(csegen_149(), $59 => Control_Monad_Dom_DomIO_n__20132_28069_handle($3, $2, $1, $0, hRef, $d, $2a.a1, $2a.a2, $2a, sfRef, $59), $0), () => Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), $2a.a2)))));
 };
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Data_IORef_newIORef(csegen_37(), {h: 0}), $c);
}

function Control_Monad_Dom_DomIO_env($0, $1) {
 return $0.a1.a2(undefined)($1);
}

function Control_Monad_Dom_DomIO_createId($0) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $a => ($0.a2.value)), n => Prelude_Interfaces_x3ex3e(csegen_124(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $17 => ($0.a2.value=(1n+n))), () => Control_Monad_Error_Either_pure_Applicative_x28x28EitherTx20x24ex29x20x24mx29(csegen_9(), Prelude_Types_String_x2bx2b($0.a1, Prelude_Show_show_Show_Nat(n)))));
}

function Control_Monad_Dom_DomIO_convR($0, $1, $2, $3) {
 return $0($2)($3)($1);
}

function Data_IORef_newIORef($0, $1) {
 return $0.a1.a2(undefined)(undefined)($0.a2(undefined)($10 => ({value:$1})))(m => $0.a1.a1.a2(undefined)(m));
}

function Rhone_JS_Sink_setValidityMessage_SetValidity_HTMLInputElement($0, $1) {
 return Web_Raw_Html_HTMLInputElement_setCustomValidity($0, $1);
}

function Rhone_JS_Sink_validityMessageAt($0, $1, $2) {
 return {h: 3, a1: $4 => Rhone_JS_Sink_setValidityMessageAt($0, $1, $2, $4)};
}

function Rhone_JS_Sink_text($0, $1) {
 return {h: 4, a1: {h: 2, a1: $4 => ({h: 2, a1: $4})}, a2: Rhone_JS_Sink_rawInnerHtml($0, $1)};
}

function Rhone_JS_Sink_setValidityMessageAt($0, $1, $2, $3) {
 return $1.a2(undefined)(Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Dom_Interface_getElementByRef($0.a1, $2), $12 => $0.a2($12)($3)));
}

function Rhone_JS_Sink_rawInnerHtml($0, $1) {
 return {h: 3, a1: $3 => Control_Monad_Dom_Interface_rawInnerHtmlAt($0, $1, Text_Html_Node_render($3))};
}

function Rhone_JS_Sink_leftInvalid($0, $1, $2) {
 return {h: 4, a1: {h: 2, a1: $5 => Prelude_Types_either(() => $8 => $8, () => $a => '', $5)}, a2: Rhone_JS_Sink_validityMessageAt($0, $1, $2)};
}

function Rhone_JS_Sink_boolAttribute($0, $1) {
 return {h: 4, a1: {h: 2, a1: $4 => ({a1: $4.a1, a2: {a1: Data_Maybe_toMaybe($4.a2.a1, () => ''), a2: {h: 0}}})}, a2: Rhone_JS_Sink_attribute($0, $1)};
}

function Rhone_JS_Sink_attribute($0, $1) {
 const $2 = $3 => {
  const $19 = el => {
   switch($3.a2.a1.h) {
    case undefined: return Web_Raw_Dom_Element_setAttribute(el, $1, $3.a2.a1.a1);
    case 0: return Web_Raw_Dom_Element_removeAttribute(el, $1);
   }
  };
  const $c = Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Dom_Interface_strictGetElementById($13 => $14 => Web_Internal_DomTypes_safeCast_SafeCast_Element($14), $3.a1.a1, $3.a1.a3), $19);
  return $0.a2(undefined)($c);
 };
 return {h: 3, a1: $2};
}

function Rhone_JS_Input_getInput($0, $1, $2, $3, $4) {
 return {h: 4, a1: Data_MSF_Util_bool($8 => $1.a1($2)($8)), a2: {h: 4, a1: {h: 8, a1: {a1: {h: 4, a1: {h: 4, a1: {h: 1, a1: $4}, a2: Rhone_JS_Source_value($0, {a1: csegen_153(), a2: $1c => Rhone_JS_Source_getValue_HasValue_HTMLInputElement($1c)})}, a2: {h: 2, a1: $21 => ({a1: $3($21)})}}, a2: {a1: Data_MSF_Util_never(), a2: {h: 0}}}}, a2: {h: 4, a1: Rhone_JS_Input_fireAndHold($3('')), a2: {h: 4, a1: {h: 5, a1: {a1: {h: 0}, a2: {a1: Data_MSF_Util_ifEvent(csegen_156(), Rhone_JS_Sink_leftInvalid({a1: csegen_153(), a2: $3e => $3f => Rhone_JS_Sink_setValidityMessage_SetValidity_HTMLInputElement($3e, $3f)}, $0, $4)), a2: {h: 0}}}}, a2: Data_MSF_Util_hd()}}}};
}

function Rhone_JS_Input_fireAndHold($0) {
 return {h: 6, a1: {a1: Data_MSF_Util_hold($0), a2: {a1: Data_MSF_Util_x3cx7cx3e({h: 0}, Data_MSF_Util_once($0)), a2: {h: 0}}}};
}

function Examples_Selector_n__16079_4090_select($0, $1) {
 switch($0) {
  case 'reset': return Control_Monad_Dom_DomIO_reactimateDom_({a1: $6 => 0}, $1.a1, Examples_Reset_ui(), $1.a2);
  case 'performance': return Control_Monad_Dom_DomIO_reactimateDom_({h: 0}, $1.a1, Examples_Performance_ui(), $1.a2);
  case 'fractals': return Control_Monad_Dom_DomIO_reactimateDom_({h: 0}, $1.a1, Examples_Fractals_ui(), $1.a2);
  default: return Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), csegen_131(), $1);
 }
}

const Examples_Selector_ui = __lazy(function () {
 return Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_rawInnerHtmlAt(csegen_171(), Examples_CSS_Core_appStyle(), Examples_CSS_allRules()), () => Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_innerHtmlAt(csegen_171(), csegen_190(), csegen_193(), Examples_CSS_Core_contentDiv(), Examples_Selector_content()), () => $1c => Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), {a1: Examples_Selector_msf(), a2: csegen_131()}, $1c)));
});

const Examples_Selector_msf = __lazy(function () {
 return {h: 9, a1: csegen_131(), a2: {h: 4, a1: {h: 5, a1: {a1: {h: 3, a1: $7 => $8 => Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28x28DomIOx20x24evx29x20x24iox29(csegen_168(), $7, $8)}, a2: {a1: {h: 3, a1: $11 => $12 => Examples_Selector_n__16079_4090_select($11, $12)}, a2: {h: 0}}}}, a2: {h: 2, a1: $19 => ({a1: $19.a2.a1, a2: {a1: 0, a2: {h: 0}}})}}};
});

const Examples_Selector_content = __lazy(function () {
 const $78 = Examples_CSS_Core_exampleDiv();
 const $77 = $78.a3;
 const $76 = {h: 0, a1: $77};
 const $75 = {a1: $76, a2: csegen_234()};
 const $72 = {h: 0, a1: 'div', a2: 17, a3: $75, a4: {h: 0}};
 const $71 = {a1: $72, a2: {h: 0}};
 const $16 = {a1: {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_contentHeader()}, a2: {h: 0}}, a4: {a1: {h: 0, a1: 'label', a2: 34, a3: csegen_210(), a4: {a1: {h: 2, a1: 'Choose an Example'}, a2: {h: 0}}}, a2: {a1: {h: 0, a1: 'Select', a2: 54, a3: {a1: Text_Html_Attribute_dispAttr('class', csegen_212(), {a1: Examples_CSS_Core_widget(), a2: {a1: Examples_CSS_Core_selectIn(), a2: {a1: Examples_CSS_Core_exampleSelector(), a2: {h: 0}}}}), a2: {a1: Text_Html_Attribute_onChange($42 => $42), a2: {h: 0}}}, a4: {a1: {h: 0, a1: 'option', a2: 45, a3: {a1: {h: 1, a1: 'value', a2: 'reset'}, a2: {a1: {h: 2, a1: 'selected', a2: 1}, a2: {h: 0}}}, a4: {a1: {h: 2, a1: 'Counting Clicks'}, a2: {h: 0}}}, a2: {a1: {h: 0, a1: 'option', a2: 45, a3: {a1: {h: 1, a1: 'value', a2: 'performance'}, a2: {h: 0}}, a4: {a1: {h: 2, a1: 'Performance'}, a2: {h: 0}}}, a2: {a1: {h: 0, a1: 'option', a2: 45, a3: {a1: {h: 1, a1: 'value', a2: 'fractals'}, a2: {h: 0}}, a4: {a1: {h: 2, a1: 'Fractals'}, a2: {h: 0}}}, a2: {h: 0}}}}}, a2: {h: 0}}}}, a2: $71};
 const $8 = {a1: {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_pageTitle()}, a2: {h: 0}}, a4: {a1: {h: 2, a1: 'rhone-js: Examples'}, a2: {h: 0}}}, a2: $16};
 return {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_contentList()}, a2: {h: 0}}, a4: $8};
});

const Examples_Reset_ui = __lazy(function () {
 return Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_innerHtmlAt(csegen_171(), csegen_190(), csegen_193(), Examples_CSS_Core_exampleDiv(), Examples_Reset_content()), () => $10 => Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), {a1: Examples_Reset_msf(), a2: csegen_131()}, $10));
});

const Examples_Reset_msf = __lazy(function () {
 return {h: 4, a1: Data_MSF_Util_accumulateWith($3 => $4 => Prelude_Basics_apply($3, $4), Number(_truncBigInt8(0n))), a2: {h: 4, a1: {h: 6, a1: {a1: {h: 4, a1: {h: 2, a1: $10 => Prelude_Show_show_Show_Int8($10)}, a2: Rhone_JS_Sink_text(csegen_171(), Examples_CSS_Reset_out())}, a2: {a1: {h: 4, a1: {h: 2, a1: $1d => Prelude_EqOrd_x3cx3d_Ord_Int8($1d, Number(_truncBigInt8(-10n)))}, a2: Data_MSF_Util_firstArg(csegen_252(), Examples_CSS_Reset_btnDec())}, a2: {a1: {h: 4, a1: {h: 2, a1: $2c => Prelude_EqOrd_x3ex3d_Ord_Int8($2c, Number(_truncBigInt8(10n)))}, a2: Data_MSF_Util_firstArg(csegen_252(), Examples_CSS_Reset_btnInc())}, a2: {a1: {h: 4, a1: {h: 2, a1: $3b => Prelude_EqOrd_x3dx3d_Eq_Int8($3b, Number(_truncBigInt8(0n)))}, a2: Data_MSF_Util_firstArg(csegen_252(), Examples_CSS_Reset_btnReset())}, a2: {h: 0}}}}}}, a2: {h: 1, a1: 0}}};
});

function Examples_Reset_line($0, $1) {
 return {h: 0, a1: 'div', a2: 17, a3: csegen_270(), a4: {a1: {h: 0, a1: 'label', a2: 34, a3: csegen_210(), a4: {a1: {h: 2, a1: $0}, a2: {h: 0}}}, a2: $1}};
}

const Examples_Reset_content = __lazy(function () {
 return {h: 0, a1: 'div', a2: 17, a3: csegen_234(), a4: {a1: Examples_Reset_line('Reset counter:', {a1: Examples_Reset_btn(Examples_CSS_Reset_btnReset(), $e => Number(_truncBigInt8(0n)), 'Reset'), a2: {h: 0}}), a2: {a1: Examples_Reset_line('Increase counter:', {a1: Examples_Reset_btn(Examples_CSS_Reset_btnInc(), $1c => _add8s($1c, 1), '+'), a2: {h: 0}}), a2: {a1: Examples_Reset_line('Decrease counter:', {a1: Examples_Reset_btn(Examples_CSS_Reset_btnDec(), $2b => _add8s($2b, -1), '-'), a2: {h: 0}}), a2: {a1: Examples_Reset_line('Count:', {a1: {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 0, a1: csegen_280()}, a2: {h: 0}}, a4: {h: 0}}, a2: {h: 0}}), a2: {h: 0}}}}}};
});

function Examples_Reset_btn($0, $1, $2) {
 return {h: 0, a1: 'button', a2: 7, a3: {a1: {h: 0, a1: $0.a3}, a2: {a1: {h: 3, a1: {h: 0, a1: $d => ({a1: $1})}}, a2: {a1: Text_Html_Attribute_dispAttr('class', csegen_212(), {a1: Examples_CSS_Core_widget(), a2: {a1: Examples_CSS_Core_btn(), a2: {a1: Examples_CSS_Reset_resetBtn(), a2: {h: 0}}}}), a2: {h: 0}}}}, a4: {a1: {h: 2, a1: $2}, a2: {h: 0}}};
}

const Examples_CSS_Reset_resetBtn = __lazy(function () {
 return 'reset_incbtn';
});

const Examples_CSS_Reset_out = __lazy(function () {
 return {a1: 'div', a2: 17, a3: 'reset_out'};
});

const Examples_CSS_Reset_css = __lazy(function () {
 return {a1: {a1: {h: 1, a1: csegen_280()}, a2: {a1: {a1: 15, a2: {h: 6}}, a2: {a1: csegen_296(), a2: {a1: {a1: 25, a2: 1}, a2: csegen_297()}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Reset_resetBtn()}, a2: {a1: csegen_296(), a2: csegen_297()}}, a2: {h: 0}}};
});

const Examples_CSS_Reset_btnReset = __lazy(function () {
 return {a1: 'button', a2: 7, a3: 'reset_reset'};
});

const Examples_CSS_Reset_btnInc = __lazy(function () {
 return {a1: 'button', a2: 7, a3: 'reset_inc'};
});

const Examples_CSS_Reset_btnDec = __lazy(function () {
 return {a1: 'button', a2: 7, a3: 'reset_dec'};
});

function Text_CSS_Selector_render($0) {
 switch($0.h) {
  case 0: return '*';
  case 1: return Prelude_Types_String_x2bx2b('#', $0.a1);
  case 2: return Prelude_Types_String_x2bx2b('.', $0.a1);
  case 3: return $0.a1;
  case 4: return Prelude_Types_fastConcat(Data_List_intersperse(', ', Prelude_Types_map_Functor_List($f => Text_CSS_Selector_render($f), $0.a1)));
  case 5: return Prelude_Types_String_x2bx2b(Text_CSS_Selector_render($0.a1), Prelude_Types_String_x2bx2b(':', Text_CSS_Selector_PseudoClass_render($0.a2)));
 }
}

function Text_CSS_Selector_PseudoClass_render($0) {
 switch($0.h) {
  case 0: return 'active';
  case 1: return 'any-link';
  case 2: return 'blank';
  case 3: return 'checked';
  case 4: return 'current';
  case 5: return 'default';
  case 6: return Prelude_Types_String_x2bx2b('dir(', Prelude_Types_String_x2bx2b(Text_CSS_Property_Direction_render($0.a1), ')'));
  case 7: return 'disabled';
  case 8: return 'empty';
  case 9: return 'enabled';
  case 10: return 'first';
  case 11: return 'first-child';
  case 12: return 'first-of-type';
  case 13: return 'focus';
  case 14: return 'focus-visible';
  case 15: return 'focus-within';
  case 16: return 'future';
  case 17: return 'hover';
  case 18: return 'indeterminate';
  case 19: return 'in-range';
  case 20: return 'invalid';
  case 21: return Prelude_Types_String_x2bx2b('lang(', Prelude_Types_String_x2bx2b($0.a1, ')'));
  case 22: return 'last-child';
  case 23: return 'last-of-type';
  case 24: return 'left';
  case 25: return 'link';
  case 26: return 'local-link';
  case 27: return Prelude_Types_String_x2bx2b('nth-child(', Prelude_Types_String_x2bx2b($0.a1, ')'));
  case 28: return Prelude_Types_String_x2bx2b('nth-of-type(', Prelude_Types_String_x2bx2b($0.a1, ')'));
  case 29: return Prelude_Types_String_x2bx2b('nth-last-child(', Prelude_Types_String_x2bx2b($0.a1, ')'));
  case 30: return Prelude_Types_String_x2bx2b('nth-last-of-type(', Prelude_Types_String_x2bx2b($0.a1, ')'));
  case 31: return 'only-child';
  case 32: return 'only-of-type';
  case 33: return 'optional';
  case 34: return 'out-of-range';
  case 35: return 'past';
  case 36: return 'placeholder-shown';
  case 37: return 'playing';
  case 38: return 'paused';
  case 39: return 'read-only';
  case 40: return 'read-write';
  case 41: return 'required';
  case 42: return 'right';
  case 43: return 'root';
  case 44: return 'scope';
  case 45: return 'valid';
  case 46: return 'target';
  case 47: return 'visited';
 }
}

function Text_CSS_Property_renderProp($0, $1) {
 switch($0) {
  case 0: return Prelude_Types_String_x2bx2b('align-items: ', Text_CSS_Flexbox_FlexAlign_render($1));
  case 1: return Prelude_Types_String_x2bx2b('align-self: ', Text_CSS_Flexbox_FlexAlign_render($1));
  case 2: return Prelude_Types_String_x2bx2b('background-color: ', Text_CSS_Color_render($1));
  case 3: return Text_CSS_Dir_render2('border', 'color', $16 => Text_CSS_Color_render($16), $1);
  case 4: return Prelude_Types_String_x2bx2b('border-radius: ', Text_CSS_Property_BorderRadius_render($1));
  case 5: return Text_CSS_Dir_render2('border', 'style', $23 => Text_CSS_Property_BorderStyle_render($23), $1);
  case 6: return Text_CSS_Dir_render2('border', 'width', $2b => Text_CSS_Property_BorderWidth_render($2b), $1);
  case 7: return Prelude_Types_String_x2bx2b('color: ', Text_CSS_Color_render($1));
  case 8: return Prelude_Types_String_x2bx2b('direction: ', Text_CSS_Property_Direction_render($1));
  case 9: return Prelude_Types_String_x2bx2b('display: ', Text_CSS_Property_Display_render($1));
  case 10: return Prelude_Types_String_x2bx2b('flex: ', $1);
  case 11: return Prelude_Types_String_x2bx2b('flex-basis: ', Text_CSS_Property_FlexBasis_render($1));
  case 13: return Prelude_Types_String_x2bx2b('flex-wrap: ', $1);
  case 12: return Prelude_Types_String_x2bx2b('flex-direction: ', Text_CSS_Flexbox_FlexDirection_render($1));
  case 14: return Prelude_Types_String_x2bx2b('font-family: ', $1);
  case 15: return Prelude_Types_String_x2bx2b('font-size: ', Text_CSS_Property_FontSize_render($1));
  case 16: return Prelude_Types_String_x2bx2b('height: ', Text_CSS_Property_Width_render($1));
  case 17: return Prelude_Types_String_x2bx2b('justify-content: ', Text_CSS_Flexbox_FlexJustify_render($1));
  case 19: return Text_CSS_Dir_render('margin', $63 => Text_CSS_Length_render($63), $1);
  case 20: return Prelude_Types_String_x2bx2b('max-height: ', Text_CSS_Property_Width_render($1));
  case 21: return Prelude_Types_String_x2bx2b('max-width: ', Text_CSS_Property_Width_render($1));
  case 22: return Prelude_Types_String_x2bx2b('min-height: ', Text_CSS_Property_Width_render($1));
  case 23: return Prelude_Types_String_x2bx2b('min-width: ', Text_CSS_Property_Width_render($1));
  case 24: return Text_CSS_Dir_render('padding', $7e => Text_CSS_Length_render($7e), $1);
  case 18: return Prelude_Types_String_x2bx2b('list-style-type: ', Text_CSS_ListStyleType_render($1));
  case 25: return Prelude_Types_String_x2bx2b('text-align: ', Text_CSS_Property_TextAlign_render($1));
  case 26: return Prelude_Types_String_x2bx2b('width: ', Text_CSS_Property_Width_render($1));
 }
}

function Text_CSS_Property_Width_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Length_render($0.a1);
  case 1: return Text_CSS_Percentage_render($0.a1);
 }
}

function Text_CSS_Property_TextAlign_render($0) {
 switch($0) {
  case 0: return 'start';
  case 1: return 'end';
  case 2: return 'left';
  case 3: return 'right';
  case 4: return 'center';
  case 5: return 'justify';
 }
}

function Text_CSS_Property_FontSize_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Length_render($0.a1);
  case 1: return Text_CSS_Percentage_render($0.a1);
  case 2: return 'xx-small';
  case 3: return 'x-small';
  case 4: return 'small';
  case 5: return 'medium';
  case 6: return 'large';
  case 7: return 'x-large';
  case 8: return 'xx-large';
  case 9: return 'xxx-large';
 }
}

function Text_CSS_Property_FlexBasis_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Length_render($0.a1);
  case 1: return Text_CSS_Percentage_render($0.a1);
 }
}

function Text_CSS_Property_Display_render($0) {
 return 'flex';
}

function Text_CSS_Property_Direction_render($0) {
 switch($0) {
  case 0: return 'ltr';
  case 1: return 'rtl';
 }
}

function Text_CSS_Property_BorderWidth_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Length_render($0.a1);
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

function Text_CSS_Property_BorderRadius_render($0) {
 switch($0.h) {
  case 0: return Text_CSS_Length_render($0.a1);
  case 1: return Text_CSS_Percentage_render($0.a1);
  case 2: return $0.a1;
 }
}

function Text_CSS_Percentage_render($0) {
 return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Bits32($0), '%');
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

function Text_CSS_Length_render($0) {
 switch($0.h) {
  case 0: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Bits16($0.a1), 'pt');
  case 1: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Bits16($0.a1), 'px');
  case 2: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Double($0.a1), 'em');
  case 3: return Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Double($0.a1), 'rem');
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

function Text_CSS_Dir_render2($0, $1, $2, $3) {
 const $4 = Prelude_Types_fastConcat(Data_List_intersperse(' ', Prelude_Types_map_Functor_List($2, Text_CSS_Dir_vals($3))));
 const $f = Text_CSS_Dir_prfx($3);
 return Prelude_Types_String_x2bx2b($0, Prelude_Types_String_x2bx2b($f, Prelude_Types_String_x2bx2b('-', Prelude_Types_String_x2bx2b($1, Prelude_Types_String_x2bx2b(': ', $4)))));
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

const Text_CSS_Color_red = __lazy(function () {
 return {a1: Number(_truncUBigInt8(255n)), a2: 0, a3: 0};
});

const Text_CSS_Color_black = __lazy(function () {
 return {a1: 0, a2: 0, a3: 0};
});

function Text_CSS_Rule_render($0) {
 return Prelude_Types_String_x2bx2b(Text_CSS_Selector_render($0.a1), Prelude_Types_String_x2bx2b('{', Prelude_Types_String_x2bx2b(Prelude_Types_fastConcat(Prelude_Types_map_Functor_List($10 => Prelude_Types_String_x2bx2b(Text_CSS_Property_renderProp($10.a1, $10.a2), ';'), $0.a2)), '}')));
}

const Examples_CSS_Core_widgetList = __lazy(function () {
 return 'widgetList';
});

const Examples_CSS_Core_widgetLine = __lazy(function () {
 return 'widgetline';
});

const Examples_CSS_Core_widgetLabel = __lazy(function () {
 return 'widgetlabel';
});

const Examples_CSS_Core_widget = __lazy(function () {
 return 'widget';
});

const Examples_CSS_Core_textIn = __lazy(function () {
 return 'textin';
});

const Examples_CSS_Core_selectIn = __lazy(function () {
 return 'selectin';
});

const Examples_CSS_Core_pageTitle = __lazy(function () {
 return 'pageTitle';
});

const Examples_CSS_Core_exampleSelector = __lazy(function () {
 return 'example_selector';
});

const Examples_CSS_Core_exampleDiv = __lazy(function () {
 return {a1: 'div', a2: 17, a3: 'example'};
});

const Examples_CSS_Core_coreCSS = __lazy(function () {
 return {a1: {a1: {h: 3, a1: 'html'}, a2: {a1: csegen_310(), a2: {h: 0}}}, a2: {a1: {a1: {h: 3, a1: 'body'}, a2: {a1: {a1: 2, a2: Text_CSS_Color_black()}, a2: {a1: {a1: 7, a2: Examples_CSS_Colors_base100()}, a2: {a1: {a1: 9, a2: 0}, a2: {a1: {a1: 12, a2: 2}, a2: {a1: {a1: 14, a2: 'Helvetica, Arial, sans-serif'}, a2: {a1: csegen_310(), a2: {a1: {a1: 19, a2: {h: 0, a1: {h: 1, a1: 0}}}, a2: {h: 0}}}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_contentList()}, a2: {a1: {a1: 1, a2: 2}, a2: {a1: {a1: 2, a2: Examples_CSS_Colors_darker_grey()}, a2: {a1: {a1: 9, a2: 0}, a2: {a1: {a1: 10, a2: '1'}, a2: {a1: {a1: 12, a2: 2}, a2: {a1: {a1: 17, a2: 3}, a2: {a1: {a1: 24, a2: {h: 5, a1: {h: 0, a1: 40}, a2: {h: 0, a1: 20}}}, a2: {a1: {a1: 21, a2: {h: 1, a1: 70}}, a2: {a1: {a1: 23, a2: {h: 1, a1: 70}}, a2: {h: 0}}}}}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_pageTitle()}, a2: {a1: {a1: 5, a2: {h: 4, a1: 4}}, a2: {a1: {a1: 6, a2: {h: 4, a1: {h: 0, a1: {h: 1, a1: 5}}}}, a2: {a1: csegen_335(), a2: {a1: {a1: 15, a2: {h: 7}}, a2: {a1: csegen_336(), a2: {a1: {a1: 24, a2: {h: 4, a1: {h: 1, a1: 60}}}, a2: {a1: {a1: 25, a2: 4}, a2: {h: 0}}}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_contentHeader()}, a2: {a1: {a1: 9, a2: 0}, a2: {a1: {a1: 5, a2: {h: 4, a1: 4}}, a2: {a1: {a1: 6, a2: {h: 4, a1: {h: 0, a1: {h: 1, a1: 2}}}}, a2: {a1: csegen_335(), a2: {a1: csegen_336(), a2: {a1: {a1: 24, a2: {h: 4, a1: {h: 1, a1: 40}}}, a2: {h: 0}}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_widget()}, a2: {a1: {a1: 2, a2: Examples_CSS_Colors_lighter_grey()}, a2: {a1: {a1: 4, a2: {h: 0, a1: {h: 1, a1: 10}}}, a2: {a1: {a1: 5, a2: {h: 0, a1: 4}}, a2: {a1: {a1: 6, a2: {h: 0, a1: {h: 0, a1: {h: 1, a1: 3}}}}, a2: {a1: {a1: 3, a2: {h: 0, a1: Examples_CSS_Colors_comp100()}}, a2: {a1: {a1: 7, a2: Examples_CSS_Colors_darker_grey()}, a2: {a1: {a1: 15, a2: {h: 6}}, a2: {a1: csegen_358(), a2: csegen_360()}}}}}}}}}, a2: {a1: {a1: {h: 5, a1: {h: 2, a1: Examples_CSS_Core_widget()}, a2: {h: 17}}, a2: csegen_373()}, a2: {a1: {a1: {h: 5, a1: {h: 2, a1: Examples_CSS_Core_widget()}, a2: {h: 0}}, a2: csegen_373()}, a2: {a1: {a1: {h: 5, a1: {h: 2, a1: Examples_CSS_Core_widget()}, a2: {h: 7}}, a2: {a1: {a1: 2, a2: Examples_CSS_Colors_light_grey()}, a2: {a1: {a1: 3, a2: {h: 0, a1: Examples_CSS_Colors_dark_grey()}}, a2: {h: 0}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_textIn()}, a2: csegen_382()}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_selectIn()}, a2: csegen_382()}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_exampleSelector()}, a2: {a1: {a1: 15, a2: {h: 6}}, a2: {h: 0}}}, a2: {a1: {a1: {h: 5, a1: {h: 2, a1: Examples_CSS_Core_widget()}, a2: {h: 20}}, a2: {a1: {a1: 3, a2: {h: 0, a1: Text_CSS_Color_red()}}, a2: {h: 0}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_widgetList()}, a2: {a1: {a1: 18, a2: 0}, a2: {a1: {a1: 9, a2: 0}, a2: {a1: {a1: 10, a2: '1'}, a2: {a1: {a1: 12, a2: 2}, a2: {a1: {a1: 17, a2: 3}, a2: {h: 0}}}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_widgetLine()}, a2: {a1: {a1: 0, a2: 5}, a2: {a1: {a1: 9, a2: 0}, a2: {a1: {a1: 19, a2: {h: 4, a1: {h: 1, a1: 5}}}, a2: {h: 0}}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Core_widgetLabel()}, a2: {a1: {a1: 15, a2: {h: 6}}, a2: csegen_402()}}, a2: {h: 0}}}}}}}}}}}}}}}}};
});

const Examples_CSS_Core_contentList = __lazy(function () {
 return 'contentList';
});

const Examples_CSS_Core_contentHeader = __lazy(function () {
 return 'contentHeader';
});

const Examples_CSS_Core_contentDiv = __lazy(function () {
 return {a1: 'body', a2: 5, a3: 'content'};
});

const Examples_CSS_Core_btn = __lazy(function () {
 return 'btn';
});

const Examples_CSS_Core_appStyle = __lazy(function () {
 return {a1: 'style', a2: 58, a3: 'appstyle'};
});

const Examples_CSS_Colors_lightest_grey = __lazy(function () {
 return {a1: 173, a2: 173, a3: 173};
});

const Examples_CSS_Colors_lighter_grey = __lazy(function () {
 return {a1: 109, a2: 109, a3: 109};
});

const Examples_CSS_Colors_light_grey = __lazy(function () {
 return {a1: 77, a2: 77, a3: 77};
});

const Examples_CSS_Colors_darker_grey = __lazy(function () {
 return {a1: 13, a2: 13, a3: 13};
});

const Examples_CSS_Colors_dark_grey = __lazy(function () {
 return {a1: 29, a2: 29, a3: 29};
});

const Examples_CSS_Colors_comp60 = __lazy(function () {
 return {a1: 92, a2: 161, a3: 230};
});

const Examples_CSS_Colors_comp100 = __lazy(function () {
 return {a1: 0, a2: 115, a3: 229};
});

const Examples_CSS_Colors_base80 = __lazy(function () {
 return {a1: 230, a2: 138, a3: 46};
});

const Examples_CSS_Colors_base100 = __lazy(function () {
 return {a1: 229, a2: 114, a3: 0};
});

function Examples_Performance_case__validate_4199($0, $1) {
 switch($1) {
  case 0n: return {h: 0, a1: Prelude_Types_String_x2bx2b('Not a positive natural number: ', $0)};
  default: return {h: 1, a1: $1};
 }
}

function Examples_Performance_validate($0) {
 return Examples_Performance_case__validate_4199($0, Prelude_Cast_cast_Cast_String_Nat($0));
}

const Examples_Performance_ui = __lazy(function () {
 return Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_innerHtmlAt(csegen_171(), csegen_190(), csegen_193(), Examples_CSS_Core_exampleDiv(), Examples_Performance_content()), () => $10 => Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), {a1: {h: 4, a1: Examples_Performance_msf(), a2: {h: 2, a1: $1a => 0}}, a2: csegen_131()}, $10));
});

const Examples_Performance_sumNats = __lazy(function () {
 return {h: 4, a1: {h: 6, a1: {a1: {h: 4, a1: Data_MSF_Util_accumulateWith($6 => $7 => Prelude_Types_x2b_Num_Nat($6, $7), 0n), a2: {h: 4, a1: {h: 2, a1: $f => Prelude_Show_show_Show_Nat($f)}, a2: Rhone_JS_Sink_text(csegen_171(), Examples_CSS_Performance_out())}}, a2: {a1: Data_MSF_Util_ifFalse(csegen_156(), $1e => Prelude_Types_x3dx3d_Eq_Nat(0n, $1e), {h: 4, a1: {h: 6, a1: {a1: {h: 2, a1: $27 => Examples_Performance_btnRef($27)}, a2: {a1: {h: 1, a1: 1}, a2: {h: 0}}}}, a2: csegen_252()}), a2: {h: 0}}}}, a2: {h: 1, a1: 0}};
});

const Examples_Performance_msf = __lazy(function () {
 return {h: 4, a1: {h: 6, a1: {a1: Examples_Performance_count(), a2: {a1: Data_MSF_Util_is(Examples_Performance_implEqEv(), 0), a2: {h: 0}}}}, a2: {h: 4, a1: Data_MSF_Util_rightOnEvent(), a2: Data_MSF_Util_ifEvent(csegen_156(), {h: 3, a1: $14 => $15 => Control_Monad_Dom_DomIO_map_Functor_x28x28DomIOx20x24evx29x20x24iox29(csegen_109(), $1a => 0, $1c => Control_Monad_Dom_DomIO_reactimateDom_({a1: 0n}, $1c.a1, $23 => Examples_Performance_btnsSF($14, $23), $1c.a2), $15)})}};
});

function Examples_Performance_line($0, $1) {
 return {h: 0, a1: 'div', a2: 17, a3: csegen_270(), a4: {a1: {h: 0, a1: 'label', a2: 34, a3: csegen_210(), a4: {a1: {h: 2, a1: $0}, a2: {h: 0}}}, a2: $1}};
}

const Examples_Performance_implGenericEv = __lazy(function () {
 const $0 = x => {
  switch(x) {
   case 0: return {h: 0, a1: {h: 0}};
   case 1: return {h: 1, a1: {h: 0, a1: {h: 0}}};
  }
 };
 const $5 = x => {
  switch(x.h) {
   case 0: return 0;
   case 1: return 1;
  }
 };
 const $a = x => {
  switch(x) {
   case 0: return 0;
   case 1: return 0;
  }
 };
 const $c = x => {
  switch(x.h) {
   case 0: return 0;
   case 1: return 0;
  }
 };
 return {a1: $0, a2: $5, a3: $a, a4: $c};
});

const Examples_Performance_implEqEv = __lazy(function () {
 const $a = a => b => {
  switch(Generics_SOP_genEq(Examples_Performance_implGenericEv(), csegen_456(), a, b)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
 return {a1: $1 => $2 => Generics_SOP_genEq(Examples_Performance_implGenericEv(), csegen_456(), $1, $2), a2: $a};
});

function Examples_Performance_dispTime($0, $1) {
 switch($0) {
  case 0n: return Prelude_Types_String_x2bx2b('Loaded ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Nat($0), Prelude_Types_String_x2bx2b(' buttons in ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Int32($1), ' ms.'))));
  default: {
   const $13 = ($0-1n);
   switch($13) {
    case 0n: return Prelude_Types_String_x2bx2b('Loaded one button in ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Int32($1), ' ms.'));
    default: return Prelude_Types_String_x2bx2b('Loaded ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Nat($0), Prelude_Types_String_x2bx2b(' buttons in ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Int32($1), ' ms.'))));
   }
  }
 }
}

const Examples_Performance_count = __lazy(function () {
 return {h: 4, a1: Rhone_JS_Input_getInput(csegen_171(), Examples_Performance_implEqEv(), 1, $8 => Examples_Performance_validate($8), Examples_CSS_Performance_natIn()), a2: Data_MSF_Util_observeWith({h: 4, a1: csegen_461(), a2: Data_MSF_Util_firstArg(csegen_252(), Examples_CSS_Performance_btnRun())})};
});

const Examples_Performance_content = __lazy(function () {
 const $f = Examples_CSS_Performance_natIn();
 const $e = $f.a3;
 const $d = {h: 0, a1: $e};
 const $c = {a1: $d, a2: {a1: Text_Html_Attribute_onInput($15 => 1), a2: {a1: Text_Html_Attribute_onEnterDown(0), a2: {a1: Text_Html_Attribute_dispAttr('class', csegen_212(), {a1: Examples_CSS_Core_widget(), a2: {a1: Examples_CSS_Performance_numButtons(), a2: {h: 0}}}), a2: {a1: {h: 1, a1: 'placeholder', a2: 'Enter a positive integer'}, a2: {h: 0}}}}}};
 const $9 = {h: 0, a1: 'input', a2: 32, a3: $c, a4: {h: 0}};
 const $8 = {a1: $9, a2: {a1: {h: 0, a1: 'button', a2: 7, a3: {a1: {h: 0, a1: csegen_476()}, a2: {a1: {h: 3, a1: {h: 0, a1: $39 => ({a1: 0})}}, a2: csegen_481()}}, a4: {a1: {h: 2, a1: 'Run'}, a2: {h: 0}}}, a2: {h: 0}}};
 const $5 = Examples_Performance_line('Number of buttons:', $8);
 const $58 = Examples_CSS_Performance_time();
 const $57 = $58.a3;
 const $56 = {h: 0, a1: $57};
 const $55 = {a1: $56, a2: csegen_270()};
 const $52 = {h: 0, a1: 'div', a2: 17, a3: $55, a4: {h: 0}};
 const $64 = Examples_CSS_Performance_buttons();
 const $63 = $64.a3;
 const $62 = {h: 0, a1: $63};
 const $61 = {a1: $62, a2: csegen_270()};
 const $5e = {h: 0, a1: 'div', a2: 17, a3: $61, a4: {h: 0}};
 const $5d = {a1: $5e, a2: {h: 0}};
 const $51 = {a1: $52, a2: $5d};
 const $42 = {a1: Examples_Performance_line('Sum:', {a1: {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 0, a1: csegen_488()}, a2: {h: 0}}, a4: {h: 0}}, a2: {h: 0}}), a2: $51};
 const $4 = {a1: $5, a2: $42};
 return {h: 0, a1: 'div', a2: 17, a3: csegen_234(), a4: $4};
});

function Examples_Performance_btnsSF($0, $1) {
 return Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_124(), csegen_507(), t1 => Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_innerHtmlAt(csegen_171(), csegen_190(), csegen_193(), Examples_CSS_Performance_buttons(), Examples_Performance_btns($0)), () => $19 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_124(), csegen_507(), t2 => Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_rawInnerHtmlAt(csegen_171(), Examples_CSS_Performance_time(), Examples_Performance_dispTime($0, _sub32s(t2, t1))), () => $30 => Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), {a1: Examples_Performance_sumNats(), a2: csegen_131()}, $30)), $19)), $1);
}

function Examples_Performance_btns($0) {
 return {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 1, a1: 'class', a2: Examples_CSS_Performance_grid()}, a2: {h: 0}}, a4: Data_List_TR_mapTR($c => Examples_Performance_btn($c), Data_List_TR_iterateTR($0, $13 => Prelude_Types_x2b_Num_Nat($13, 1n), 1n))};
}

function Examples_Performance_btnRef($0) {
 return {a1: 'button', a2: 7, a3: Prelude_Types_String_x2bx2b('BTN', Prelude_Show_show_Show_Nat($0))};
}

function Examples_Performance_btn($0) {
 const $6 = Examples_Performance_btnRef($0);
 const $5 = $6.a3;
 const $4 = {h: 0, a1: $5};
 const $3 = {a1: $4, a2: {a1: {h: 3, a1: {h: 0, a1: $d => ({a1: $0})}}, a2: {a1: Text_Html_Attribute_dispAttr('class', csegen_212(), {a1: Examples_CSS_Core_widget(), a2: {a1: Examples_CSS_Core_btn(), a2: {a1: Examples_CSS_Performance_inc(), a2: {h: 0}}}}), a2: {h: 0}}}};
 return {h: 0, a1: 'button', a2: 7, a3: $3, a4: {a1: {h: 2, a1: Prelude_Show_show_Show_Nat($0)}, a2: {h: 0}}};
}

function Generics_SOP_genEq($0, $1, $2, $3) {
 return Data_SOP_SOP_x3dx3d_Eq_x28x28x28SOP_x20x24kx29x20x24fx29x20x24kssx29($1, $0.a1($2), $0.a1($3));
}

const Examples_CSS_Performance_time = __lazy(function () {
 return {a1: 'div', a2: 17, a3: 'performance_time'};
});

const Examples_CSS_Performance_out = __lazy(function () {
 return {a1: 'div', a2: 17, a3: 'performance_sum'};
});

const Examples_CSS_Performance_numButtons = __lazy(function () {
 return 'performance_numbuttons';
});

const Examples_CSS_Performance_natIn = __lazy(function () {
 return {a1: 'input', a2: 32, a3: 'performance_numbuttons'};
});

const Examples_CSS_Performance_inc = __lazy(function () {
 return 'performance_inc';
});

const Examples_CSS_Performance_grid = __lazy(function () {
 return 'performance_grid';
});

const Examples_CSS_Performance_css = __lazy(function () {
 return {a1: {a1: {h: 1, a1: csegen_488()}, a2: {a1: {a1: 15, a2: {h: 6}}, a2: csegen_518()}}, a2: {a1: {a1: {h: 1, a1: csegen_476()}, a2: csegen_297()}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Performance_grid()}, a2: {a1: {a1: 9, a2: 0}, a2: {a1: {a1: 13, a2: 'wrap'}, a2: {h: 0}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Performance_inc()}, a2: {a1: {a1: 11, a2: {h: 1, a1: 5}}, a2: {a1: {a1: 15, a2: {h: 2}}, a2: {h: 0}}}}, a2: {a1: {a1: {h: 2, a1: Examples_CSS_Performance_numButtons()}, a2: {a1: csegen_359(), a2: {a1: csegen_358(), a2: csegen_517()}}}, a2: {h: 0}}}}}};
});

const Examples_CSS_Performance_buttons = __lazy(function () {
 return {a1: 'div', a2: 17, a3: 'performance_buttons'};
});

const Examples_CSS_Performance_btnRun = __lazy(function () {
 return {a1: 'button', a2: 7, a3: 'performance_run'};
});

function Data_List_TR_mapTR($0, $1) {
 return Data_List_TR_n__3157_2277_run($0, {h: 0}, $1);
}

function Data_List_TR_iterateTR($0, $1, $2) {
 return Data_List_TR_n__3136_2253_run($1, $0, {h: 0}, $0, $2);
}

function Examples_Fractals_Iterations_case__read_4871($0, $1) {
 switch($1) {
  case 0n: return {h: 0, a1: Prelude_Types_String_x2bx2b('Not a natural number: ', $0)};
  default: {
   const $7 = Data_Nat_isLTE($1, Examples_Fractals_MaxIter());
   switch($7.h) {
    case 0: return {h: 1, a1: $1};
    case 1: return {h: 0, a1: Prelude_Types_String_x2bx2b('Value must be <= ', Prelude_Show_show_Show_Nat(Examples_Fractals_MaxIter()))};
   }
  }
 }
}

function Examples_Fractals_n__24329_5398_timer($0, $1, $2) {
 return Prelude_Interfaces_x3ex3e(csegen_124(), Examples_Fractals_n__24329_5397_cleanup($0, $1), () => Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Examples_Fractals_setInterval(csegen_37(), $2, $1({h: 4})), newID => Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $1b => ($0.value={a1: newID}))));
}

function Examples_Fractals_n__23973_4998_readAll($0) {
 const $1f = b => a => func => $20 => {
  switch($20.h) {
   case 0: return {h: 0, a1: $20.a1};
   case 1: return {h: 1, a1: func($20.a1)};
  }
 };
 const $29 = b => a => $2a => $2b => {
  switch($2a.h) {
   case 0: return {h: 0, a1: $2a.a1};
   case 1: {
    switch($2b.h) {
     case 1: return {h: 1, a1: $2a.a1($2b.a1)};
     case 0: return {h: 0, a1: $2b.a1};
    }
   }
  }
 };
 const $1e = {a1: $1f, a2: a => $27 => ({h: 1, a1: $27}), a3: $29};
 const $3d = $3e => $3f => $40 => $41 => {
  switch($41.h) {
   case 0: return {h: 0, a1: $41.a1};
   case 1: return {h: 1, a1: $40($41.a1)};
  }
 };
 const $33 = Control_Applicative_Syntax_x3cx24x24x3e($36 => $37 => $38 => $39 => ({h: 4, a1: $39, a2: {h: 2, a1: $38}}), $3d, $48 => $49 => ({a1: 0, a2: $48, a3: $49}), Rhone_JS_Input_getInput(csegen_171(), Examples_Fractals_implEqEv(), {h: 1}, $55 => Examples_Fractals_Iterations_read($55), Examples_CSS_Fractals_txtIter()));
 const $1 = Control_Applicative_Syntax_x3cx2ax2ax3e({a1: b => a => func => $5 => ({h: 4, a1: $5, a2: {h: 2, a1: func}}), a2: a => $a => ({h: 1, a1: $a}), a3: b => a => $d => $e => ({h: 4, a1: {h: 6, a1: {a1: $d, a2: {a1: $e, a2: {h: 0}}}}, a2: {h: 2, a1: $17 => Prelude_Basics_apply($17.a1, $17.a2.a1)}})}, $1e, $33, Rhone_JS_Input_getInput(csegen_171(), Examples_Fractals_implEqEv(), {h: 2}, $62 => Examples_Fractals_RedrawAfter_read($62), Examples_CSS_Fractals_txtRedraw()));
 return {h: 4, a1: $1, a2: Data_MSF_Util_observeWith({h: 4, a1: csegen_461(), a2: Data_MSF_Util_firstArg(csegen_252(), Examples_CSS_Fractals_btnRun())})};
}

function Examples_Fractals_n__23973_4997_fractal($0, $1) {
 return Data_MSF_Util_ifIs(Examples_Fractals_implEqEv(), csegen_156(), {h: 4}, {h: 4, a1: Data_MSF_Util_cycle(Examples_Fractals_Dragon_mkDragons($1.a2)), a2: {h: 3, a1: $11 => Control_Monad_Dom_Interface_rawInnerHtmlAt(csegen_171(), Examples_CSS_Fractals_out(), $11)}});
}

function Examples_Fractals_n__23973_4999_config($0) {
 return {h: 4, a1: {h: 6, a1: {a1: Examples_Fractals_n__23973_4998_readAll($0), a2: {a1: Data_MSF_Util_is(Examples_Fractals_implEqEv(), {h: 3}), a2: {h: 0}}}}, a2: {h: 4, a1: Data_MSF_Util_rightOnEvent(), a2: Data_MSF_Util_observeWith(Data_MSF_Util_ifEvent(csegen_156(), {h: 3, a1: $18 => $19 => Control_Monad_Dom_DomIO_liftJSIO_LiftJSIO_x28x28DomIOx20x24evx29x20x24iox29(csegen_168(), $0($18.a3), $19)}))}};
}

function Examples_Fractals_n__24329_5397_cleanup($0, $1) {
 return Control_Monad_Error_Either_x3ex3ex3d_Monad_x28x28EitherTx20x24ex29x20x24mx29(csegen_15(), Control_Monad_Error_Either_liftIO_HasIO_x28x28EitherTx20x24ex29x20x24mx29(csegen_34(), $a => ($0.value)), $f => Prelude_Interfaces_traverse_(csegen_149(), $14 => Examples_Fractals_clearInterval(csegen_37(), $14), $f));
}

const Examples_Fractals_ui = __lazy(function () {
 return Prelude_Interfaces_x3ex3e(csegen_130(), Control_Monad_Dom_Interface_innerHtmlAt(csegen_171(), csegen_190(), csegen_193(), Examples_CSS_Core_exampleDiv(), Examples_Fractals_content()), () => $10 => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_124(), Data_IORef_newIORef(csegen_167(), {h: 0}), ref => $1a => Control_Monad_Dom_DomIO_x3ex3ex3d_Monad_x28x28DomIOx20x24evx29x20x24iox29(csegen_124(), Prelude_Interfaces_x3cx24x3e($21 => $22 => $23 => $24 => $25 => Control_Monad_Dom_DomIO_map_Functor_x28x28DomIOx20x24evx29x20x24iox29(csegen_109(), $23, $24, $25), $2d => $2d.a3, $30 => Control_Monad_Dom_DomIO_env(csegen_124(), $30)), h => $36 => Control_Monad_Dom_DomIO_pure_Applicative_x28x28DomIOx20x24evx29x20x24iox29(csegen_24(), {a1: Examples_Fractals_msf($3e => Examples_Fractals_n__24329_5398_timer(ref, h, $3e)), a2: Examples_Fractals_n__24329_5397_cleanup(ref, h)}, $36), $1a), $10));
});

function Examples_Fractals_setInterval($0, $1, $2) {
 return $0.a2(undefined)($8 => Examples_Fractals_prim__setInterval($1, $c => JS_Util_runJS($2, $c), $8));
}

function Examples_Fractals_RedrawAfter_read($0) {
 const $1 = Prelude_Cast_cast_Cast_String_Bits32($0);
 const $4 = Data_So_choose(Examples_Fractals_isDelay($1));
 switch($4.h) {
  case 0: return {h: 1, a1: $1};
  case 1: return {h: 0, a1: 'Enter a value between 100 and 10\'000'};
 }
}

function Examples_Fractals_Iterations_read($0) {
 switch($0) {
  case '0': return {h: 1, a1: 0n};
  default: return Examples_Fractals_Iterations_case__read_4871($0, Prelude_Cast_cast_Cast_String_Nat($0));
 }
}

function Examples_Fractals_msf($0) {
 return Data_MSF_Switch_rswitchWhen({h: 1, a1: 0}, Examples_Fractals_n__23973_4999_config($0), $8 => Examples_Fractals_n__23973_4997_fractal($0, $8));
}

function Examples_Fractals_line($0, $1) {
 return {h: 0, a1: 'div', a2: 17, a3: csegen_270(), a4: {a1: {h: 0, a1: 'label', a2: 34, a3: csegen_210(), a4: {a1: {h: 2, a1: $0}, a2: {h: 0}}}, a2: $1}};
}

function Examples_Fractals_isDelay($0) {
 switch(Prelude_EqOrd_x3cx3d_Ord_Bits32(Number(_truncUBigInt32(100n)), $0)) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Bits32($0, Number(_truncUBigInt32(10000n)));
  case 0: return 0;
 }
}

const Examples_Fractals_implGenericFractal = __lazy(function () {
 return {a1: x => ({h: 0, a1: {h: 0}}), a2: csegen_569(), a3: x => 0, a4: csegen_569()};
});

const Examples_Fractals_implGenericEv = __lazy(function () {
 const $0 = x => {
  switch(x.h) {
   case 0: return {h: 0, a1: {a1: x.a1, a2: {h: 0}}};
   case 1: return {h: 1, a1: {h: 0, a1: {h: 0}}};
   case 2: return {h: 1, a1: {h: 1, a1: {h: 0, a1: {h: 0}}}};
   case 3: return csegen_572();
   case 4: return {h: 1, a1: csegen_572()};
  }
 };
 const $d = x => {
  switch(x.h) {
   case 0: return {h: 0, a1: x.a1.a1};
   case 1: {
    switch(x.a1.h) {
     case 0: return {h: 1};
     case 1: {
      switch(x.a1.a1.h) {
       case 0: return {h: 2};
       case 1: {
        switch(x.a1.a1.a1.h) {
         case 0: return {h: 3};
         case 1: return {h: 4};
        }
       }
      }
     }
    }
   }
  }
 };
 const $1a = x => {
  switch(x.h) {
   case 0: return 0;
   case 1: return 0;
   case 2: return 0;
   case 3: return 0;
   case 4: return 0;
  }
 };
 const $1c = x => {
  switch(x.h) {
   case 0: return 0;
   case 1: {
    switch(x.a1.h) {
     case 0: return 0;
     case 1: {
      switch(x.a1.a1.h) {
       case 0: return 0;
       case 1: {
        switch(x.a1.a1.a1.h) {
         case 0: return 0;
         case 1: return 0;
        }
       }
      }
     }
    }
   }
  }
 };
 return {a1: $0, a2: $d, a3: $1a, a4: $1c};
});

const Examples_Fractals_implEqFractal = __lazy(function () {
 const $b = a => b => {
  switch(Generics_SOP_genEq(Examples_Fractals_implGenericFractal(), {a1: {h: 0}, a2: {h: 0}}, a, b)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
 return {a1: $1 => $2 => Generics_SOP_genEq(Examples_Fractals_implGenericFractal(), {a1: {h: 0}, a2: {h: 0}}, $1, $2), a2: $b};
});

const Examples_Fractals_implEqEv = __lazy(function () {
 const $a = a => b => {
  switch(Generics_SOP_genEq(Examples_Fractals_implGenericEv(), csegen_582(), a, b)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
 return {a1: $1 => $2 => Generics_SOP_genEq(Examples_Fractals_implGenericEv(), csegen_582(), $1, $2), a2: $a};
});

const Examples_Fractals_content = __lazy(function () {
 return {h: 0, a1: 'div', a2: 17, a3: csegen_234(), a4: {a1: Examples_Fractals_line('Number of iterations:', {a1: {h: 0, a1: 'input', a2: 32, a3: {a1: {h: 0, a1: csegen_586()}, a2: {a1: Text_Html_Attribute_onInput($14 => ({h: 1})), a2: {a1: Text_Html_Attribute_onEnterDown({h: 3}), a2: {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_widget()}, a2: {a1: {h: 1, a1: 'placeholder', a2: Prelude_Types_String_x2bx2b('Range: [0, ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Nat(Examples_Fractals_MaxIter()), ']'))}, a2: {h: 0}}}}}}, a4: {h: 0}}, a2: {h: 0}}), a2: {a1: Examples_Fractals_line('Iteration delay [ms]:', {a1: {h: 0, a1: 'input', a2: 32, a3: {a1: {h: 0, a1: csegen_599()}, a2: {a1: Text_Html_Attribute_onInput($3e => ({h: 2})), a2: {a1: Text_Html_Attribute_onEnterDown({h: 3}), a2: {a1: {h: 1, a1: 'class', a2: Examples_CSS_Core_widget()}, a2: {a1: {h: 1, a1: 'placeholder', a2: 'Range: [100,10\'000]'}, a2: {h: 0}}}}}}, a4: {h: 0}}, a2: {a1: {h: 0, a1: 'button', a2: 7, a3: {a1: {h: 0, a1: csegen_607()}, a2: {a1: {h: 3, a1: {h: 0, a1: $5a => ({a1: {h: 3}})}}, a2: csegen_481()}}, a4: {a1: {h: 2, a1: 'Run'}, a2: {h: 0}}}, a2: {h: 0}}}), a2: {a1: {h: 0, a1: 'div', a2: 17, a3: {a1: {h: 0, a1: csegen_616()}, a2: {h: 0}}, a4: {h: 0}}, a2: {h: 0}}}}};
});

function Examples_Fractals_clearInterval($0, $1) {
 return $0.a2(undefined)($7 => Examples_Fractals_prim__clearInterval($1, $7));
}

const Examples_Fractals_MaxIter = __lazy(function () {
 return 18n;
});

function Examples_Fractals_Dragon_n__3242_4094_go($0, $1, $2, $3) {
 switch($2) {
  case 0n: return {h: 0};
  default: {
   const $5 = ($2-1n);
   const $8 = Examples_Fractals_Dragon_nextDragon($1);
   return {a1: Examples_Fractals_Dragon_dragonSVG($3, $8), a2: Examples_Fractals_Dragon_n__3242_4094_go($0, $8, $5, Prelude_Types_x2b_Num_Nat($3, 1n))};
  }
 }
}

function Examples_Fractals_Dragon_fromInteger_Num_Point($0) {
 return {a1: Number(_truncBigInt32($0)), a2: 0};
}

function Examples_Fractals_Dragon_x2d_Neg_Point($0, $1) {
 return {a1: _sub32s($0.a1, $1.a1), a2: _sub32s($0.a2, $1.a2)};
}

function Examples_Fractals_Dragon_x2b_Num_Point($0, $1) {
 return {a1: _add32s($0.a1, $1.a1), a2: _add32s($0.a2, $1.a2)};
}

function Examples_Fractals_Dragon_rotateAround90($0, $1) {
 return Examples_Fractals_Dragon_x2b_Num_Point(Examples_Fractals_Dragon_rotate90(Examples_Fractals_Dragon_x2d_Neg_Point($1, $0)), $0);
}

function Examples_Fractals_Dragon_rotate90($0) {
 return {a1: $0.a2, a2: _sub32s(0, $0.a1)};
}

function Examples_Fractals_Dragon_nextDragon($0) {
 const $2 = Data_List_TR_mapTR($5 => Examples_Fractals_Dragon_rotateAround90($0.a1, $5), Prelude_Types_List_reverse($0.a2));
 return Prelude_Types_List_tailRecAppend($2, {a1: $0.a1, a2: $0.a2});
}

function Examples_Fractals_Dragon_mkDragons($0) {
 const $1 = Examples_Fractals_Dragon_firstDragon();
 return {a1: Examples_Fractals_Dragon_dragonSVG(0n, $1), a2: Examples_Fractals_Dragon_n__3242_4094_go($0, $1, $0, 1n)};
}

const Examples_Fractals_Dragon_firstDragon = __lazy(function () {
 return {a1: {a1: 0, a2: 0}, a2: {a1: Examples_Fractals_Dragon_fromInteger_Num_Point(800n), a2: {h: 0}}};
});

function Examples_Fractals_Dragon_dragonSVG($0, $1) {
 const $2 = Prelude_Types_pow(2.0, Prelude_Num_x2f_Fractional_Double(Prelude_Cast_cast_Cast_Nat_Double(Prelude_Types_x2b_Num_Nat($0, 2n)), 2.0));
 const $e = Prelude_Num_x2f_Fractional_Double(1.0, $2);
 const $12 = Prelude_Types_fastConcat(Data_List_TR_mapTR($17 => Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Int32($17.a1), Prelude_Types_String_x2bx2b(', ', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Int32($17.a2), ' '))), $1));
 const $27 = '<svg version=\"1.1\"\n     width=\"100%\"\n     viewBox=\"0 0 1000 1000\"\n     xmlns=\"http://www.w3.org/2000/svg\">';
 return Prelude_Types_String_x2bx2b($27, Prelude_Types_String_x2bx2b('\n  <polyline points=\"', Prelude_Types_String_x2bx2b($12, Prelude_Types_String_x2bx2b('\"\n            fill=\"none\"\n            stroke=\"red\"\n            transform=\"translate (500,500) scale(', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Double($e), Prelude_Types_String_x2bx2b(')\"\n            stroke-width=\"', Prelude_Types_String_x2bx2b(Prelude_Show_show_Show_Double($2), '\"/>\n</svg>')))))));
}

const Examples_CSS_Fractals_txtRedraw = __lazy(function () {
 return {a1: 'input', a2: 32, a3: 'fractals_redrawdelay'};
});

const Examples_CSS_Fractals_txtIter = __lazy(function () {
 return {a1: 'input', a2: 32, a3: 'fractals_iterations'};
});

const Examples_CSS_Fractals_out = __lazy(function () {
 return {a1: 'div', a2: 17, a3: 'fractals_out'};
});

const Examples_CSS_Fractals_css = __lazy(function () {
 return {a1: {a1: {h: 1, a1: csegen_586()}, a2: csegen_518()}, a2: {a1: {a1: {h: 1, a1: csegen_599()}, a2: csegen_518()}, a2: {a1: {a1: {h: 1, a1: csegen_607()}, a2: csegen_297()}, a2: {a1: {a1: {h: 1, a1: csegen_616()}, a2: {a1: {a1: 10, a2: '1'}, a2: csegen_360()}}, a2: {h: 0}}}}};
});

const Examples_CSS_Fractals_btnRun = __lazy(function () {
 return {a1: 'button', a2: 7, a3: 'fractals_run'};
});

function Data_So_choose($0) {
 switch($0) {
  case 1: return {h: 0, a1: 0};
  case 0: return {h: 1, a1: 0};
 }
}

function Data_MSF_Switch_n__4055_6236_next($0, $1, $2, $3) {
 switch($3.a2.a1.a2.a1.h) {
  case 0: return {h: 1, a1: $3.a1};
  case undefined: return {h: 0, a1: {a1: {a1: $3.a2.a1.a1, a2: {a1: $3.a2.a1.a2.a1.a1, a2: {h: 0}}}, a2: $3.a1}};
 }
}

function Data_MSF_Switch_n__4055_6237_cont($0, $1, $2, $3) {
 return Data_MSF_Switch_rswitchWhen($0($3.a2.a1), $3.a1, $0);
}

function Data_MSF_Switch_rswitchWhen($0, $1, $2) {
 return {h: 12, a1: {h: 4, a1: {h: 6, a1: {a1: $0, a2: {a1: {h: 13, a1: $1}, a2: {h: 0}}}}, a2: {h: 2, a1: $d => Data_MSF_Switch_n__4055_6236_next($2, $1, $0, $d)}}, a2: () => $14 => Data_MSF_Switch_n__4055_6237_cont($2, $1, $0, $14)};
}

function Control_Applicative_Syntax_x3cx2ax2ax3e($0, $1, $2, $3) {
 return $0.a3(undefined)(undefined)($0.a3(undefined)(undefined)($0.a2(undefined)($19 => $1a => $1.a3(undefined)(undefined)($19)($1a)))($2))($3);
}

function Control_Applicative_Syntax_x3cx24x24x3e($0, $1, $2, $3) {
 return $0(undefined)(undefined)($b => $1(undefined)(undefined)($2)($b))($3);
}

const Examples_CSS_allRules = __lazy(function () {
 return Data_String_fastUnlines(Prelude_Types_map_Functor_List($4 => Text_CSS_Rule_render($4), Prelude_Types_List_tailRecAppend(Examples_CSS_Core_coreCSS(), Prelude_Types_List_tailRecAppend(Examples_CSS_Fractals_css(), Prelude_Types_List_tailRecAppend(Examples_CSS_Performance_css(), Examples_CSS_Reset_css())))));
});


try{__mainExpression_0()}catch(e){if(e instanceof IdrisError){console.log('ERROR: ' + e.message)}else{throw e} }
