(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.cu.aR === region.cU.aR)
	{
		return 'on line ' + region.cu.aR;
	}
	return 'on lines ' + region.cu.aR + ' through ' + region.cU.aR;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eg,
		impl.e3,
		impl.eW,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		u: func(record.u),
		by: record.by,
		br: record.br
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.u;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.by;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.br) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eg,
		impl.e3,
		impl.eW,
		function(sendToApp, initialModel) {
			var view = impl.e5;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eg,
		impl.e3,
		impl.eW,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.cs && impl.cs(sendToApp)
			var view = impl.e5;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.H);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.ag) && (_VirtualDom_doc.title = title = doc.ag);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.eA;
	var onUrlRequest = impl.eB;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		cs: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.dl === next.dl
							&& curr.c_ === next.c_
							&& curr.dj.a === next.dj.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		eg: function(flags)
		{
			return A3(impl.eg, flags, _Browser_getUrl(), key);
		},
		e5: impl.e5,
		e3: impl.e3,
		eW: impl.eW
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { ea: 'hidden', dU: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { ea: 'mozHidden', dU: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { ea: 'msHidden', dU: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { ea: 'webkitHidden', dU: 'webkitvisibilitychange' }
		: { ea: 'hidden', dU: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		T: _Browser_getScene(),
		X: {
			E: _Browser_window.pageXOffset,
			F: _Browser_window.pageYOffset,
			g: _Browser_doc.documentElement.clientWidth,
			f: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		g: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		f: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			T: {
				g: node.scrollWidth,
				f: node.scrollHeight
			},
			X: {
				E: node.scrollLeft,
				F: node.scrollTop,
				g: node.clientWidth,
				f: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			T: _Browser_getScene(),
			X: {
				E: x,
				F: y,
				g: _Browser_doc.documentElement.clientWidth,
				f: _Browser_doc.documentElement.clientHeight
			},
			aN: {
				E: x + rect.left,
				F: y + rect.top,
				g: rect.width,
				f: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.J.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.J.b, xhr)); });
		$elm$core$Maybe$isJust(request.dG) && _Http_track(router, xhr, request.dG.a);

		try {
			xhr.open(request.c6, request.O, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.O));
		}

		_Http_configureRequest(xhr, request);

		request.H.a && xhr.setRequestHeader('Content-Type', request.H.a);
		xhr.send(request.H.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.cZ; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.dE.a || 0;
	xhr.responseType = request.J.d;
	xhr.withCredentials = request.dL;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		O: xhr.responseURL,
		eU: xhr.status,
		eV: xhr.statusText,
		cZ: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			eS: event.loaded,
			N: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			eJ: event.loaded,
			N: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.j) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.n),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.n);
		} else {
			var treeLen = builder.j * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.p) : builder.p;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.j);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.n) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.n);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{p: nodeList, j: (len / $elm$core$Array$branchFactor) | 0, n: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {cV: fragment, c_: host, di: path, dj: port_, dl: protocol, dm: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$apiKeyFromFlags = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2($elm$json$Json$Decode$field, 'apiKey', $elm$json$Json$Decode$string),
		flags);
	if (!_v0.$) {
		var apiKey = _v0.a;
		return $elm$core$Maybe$Just(apiKey);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$clientIdFromFlags = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2($elm$json$Json$Decode$field, 'clientId', $elm$json$Json$Decode$string),
		flags);
	if (!_v0.$) {
		var clientId = _v0.a;
		return $elm$core$Maybe$Just(clientId);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $author$project$FilterTag$FilterTag = F2(
	function (tag, isActive) {
		return {aQ: isActive, Z: tag};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$FilterTag$decode = A3(
	$elm$json$Json$Decode$map2,
	$author$project$FilterTag$FilterTag,
	A2($elm$json$Json$Decode$field, 'tag', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'isActive', $elm$json$Json$Decode$bool));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Main$filterTagsFromLocalStorage = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['localStore', 'filterTags']),
			$elm$json$Json$Decode$list($author$project$FilterTag$decode)),
		flags);
	if (!_v0.$) {
		var data = _v0.a;
		return $elm$core$Maybe$Just(data);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$ItemData = function (id) {
	return function (title) {
		return function (tags) {
			return function (draftTitle) {
				return function (draftTags) {
					return function (draftTagsInput) {
						return function (draftChanged) {
							return function (done) {
								return function (orderIndexDefault) {
									return function (orderIndexOverride) {
										return function (editing) {
											return function (synced) {
												return function (_new) {
													return function (lastSyncedRevision) {
														return function (clientRevision) {
															return function (oldId) {
																return {aI: clientRevision, ad: done, aj: draftChanged, aw: draftTags, ax: draftTagsInput, ay: draftTitle, aM: editing, aA: id, b1: lastSyncedRevision, b6: _new, aV: oldId, bp: orderIndexDefault, ch: orderIndexOverride, bA: synced, _: tags, ag: title};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $author$project$ItemData$decodeApply = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $author$project$ItemData$required = F3(
	function (fieldName, itemDecoder, functionDecoder) {
		return A2(
			$author$project$ItemData$decodeApply,
			A2($elm$json$Json$Decode$field, fieldName, itemDecoder),
			functionDecoder);
	});
var $author$project$ItemData$decode = A3(
	$author$project$ItemData$required,
	'oldId',
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$null('')
			])),
	A3(
		$author$project$ItemData$required,
		'clientRevision',
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int),
		A3(
			$author$project$ItemData$required,
			'lastSyncedRevision',
			$elm$json$Json$Decode$int,
			A3(
				$author$project$ItemData$required,
				'new',
				$elm$json$Json$Decode$bool,
				A3(
					$author$project$ItemData$required,
					'synced',
					$elm$json$Json$Decode$bool,
					A3(
						$author$project$ItemData$required,
						'editing',
						$elm$json$Json$Decode$bool,
						A3(
							$author$project$ItemData$required,
							'orderIndexOverride',
							$elm$json$Json$Decode$int,
							A3(
								$author$project$ItemData$required,
								'orderIndexDefault',
								$elm$json$Json$Decode$int,
								A3(
									$author$project$ItemData$required,
									'done',
									$elm$json$Json$Decode$int,
									A3(
										$author$project$ItemData$required,
										'draftChanged',
										$elm$json$Json$Decode$bool,
										A3(
											$author$project$ItemData$required,
											'draftTagsInput',
											$elm$json$Json$Decode$string,
											A3(
												$author$project$ItemData$required,
												'draftTags',
												$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
												A3(
													$author$project$ItemData$required,
													'draftTitle',
													$elm$json$Json$Decode$string,
													A3(
														$author$project$ItemData$required,
														'tags',
														$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
														A3(
															$author$project$ItemData$required,
															'title',
															$elm$json$Json$Decode$string,
															A3(
																$author$project$ItemData$required,
																'id',
																$elm$json$Json$Decode$string,
																$elm$json$Json$Decode$succeed($author$project$ItemData$ItemData)))))))))))))))));
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $author$project$Main$itemsFromLocalStorage = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['localStore', 'items']),
			$elm$json$Json$Decode$dict($author$project$ItemData$decode)),
		flags);
	if (!_v0.$) {
		var data = _v0.a;
		return $elm$core$Maybe$Just(data);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$noTagsFilterFromLocalStorage = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['localStore', 'noTagsFilterActive']),
			$elm$json$Json$Decode$bool),
		flags);
	if (!_v0.$) {
		var data = _v0.a;
		return $elm$core$Maybe$Just(data);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$overrideOrderingFromLocalStorage = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['localStore', 'overrideOrdering']),
			$elm$json$Json$Decode$bool),
		flags);
	if (!_v0.$) {
		var data = _v0.a;
		return $elm$core$Maybe$Just(data);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Main$ItemsReceived = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$ItemData$encode = function (_v0) {
	var id = _v0.aA;
	var title = _v0.ag;
	var tags = _v0._;
	var draftTitle = _v0.ay;
	var draftTags = _v0.aw;
	var draftTagsInput = _v0.ax;
	var draftChanged = _v0.aj;
	var done = _v0.ad;
	var orderIndexDefault = _v0.bp;
	var orderIndexOverride = _v0.ch;
	var editing = _v0.aM;
	var synced = _v0.bA;
	var _new = _v0.b6;
	var lastSyncedRevision = _v0.b1;
	var clientRevision = _v0.aI;
	var oldId = _v0.aV;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$string(id)),
				_Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(title)),
				_Utils_Tuple2(
				'tags',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, tags)),
				_Utils_Tuple2(
				'draftTitle',
				$elm$json$Json$Encode$string(draftTitle)),
				_Utils_Tuple2(
				'draftTags',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, draftTags)),
				_Utils_Tuple2(
				'draftTagsInput',
				$elm$json$Json$Encode$string(draftTagsInput)),
				_Utils_Tuple2(
				'draftChanged',
				$elm$json$Json$Encode$bool(draftChanged)),
				_Utils_Tuple2(
				'done',
				$elm$json$Json$Encode$int(done)),
				_Utils_Tuple2(
				'orderIndexDefault',
				$elm$json$Json$Encode$int(orderIndexDefault)),
				_Utils_Tuple2(
				'orderIndexOverride',
				$elm$json$Json$Encode$int(orderIndexOverride)),
				_Utils_Tuple2(
				'editing',
				$elm$json$Json$Encode$bool(editing)),
				_Utils_Tuple2(
				'synced',
				$elm$json$Json$Encode$bool(synced)),
				_Utils_Tuple2(
				'new',
				$elm$json$Json$Encode$bool(_new)),
				_Utils_Tuple2(
				'lastSyncedRevision',
				$elm$json$Json$Encode$int(lastSyncedRevision)),
				_Utils_Tuple2(
				'clientRevision',
				function () {
					if (!clientRevision.$) {
						var clRev = clientRevision.a;
						return $elm$json$Json$Encode$int(clRev);
					} else {
						return $elm$json$Json$Encode$null;
					}
				}()),
				_Utils_Tuple2(
				'oldId',
				$elm$json$Json$Encode$string(oldId))
			]));
};
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.eU));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectString = function (toMsg) {
	return A2(
		$elm$http$Http$expectStringResponse,
		toMsg,
		$elm$http$Http$resolve($elm$core$Result$Ok));
};
var $author$project$Urls$backendBaseUrl = 'https://picluster.a-h.wtf/einkaufsliste-multiuser/api/v1';
var $author$project$Urls$itemsSyncUrl = F2(
	function (apiKey, clientId) {
		return $author$project$Urls$backendBaseUrl + ('/items/sync?k=' + (apiKey + ('&c=' + clientId)));
	});
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {$7: reqs, aE: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.dG;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.$7));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.aE)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					dL: r.dL,
					H: r.H,
					J: A2(_Http_mapExpect, func, r.J),
					cZ: r.cZ,
					c6: r.c6,
					dE: r.dE,
					dG: r.dG,
					O: r.O
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{dL: false, H: r.H, J: r.J, cZ: r.cZ, c6: r.c6, dE: r.dE, dG: r.dG, O: r.O}));
};
var $elm$http$Http$post = function (r) {
	return $elm$http$Http$request(
		{H: r.H, J: r.J, cZ: _List_Nil, c6: 'POST', dE: $elm$core$Maybe$Nothing, dG: $elm$core$Maybe$Nothing, O: r.O});
};
var $author$project$Main$syncItems = F3(
	function (apiKey, clientId, items) {
		return $elm$http$Http$post(
			{
				H: $elm$http$Http$jsonBody(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'clientItems',
								A2($elm$json$Json$Encode$list, $author$project$ItemData$encode, items))
							]))),
				J: $elm$http$Http$expectString($author$project$Main$ItemsReceived),
				O: A2($author$project$Urls$itemsSyncUrl, apiKey, clientId)
			});
	});
var $author$project$Main$userAgentFromFlags = function (flags) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A2($elm$json$Json$Decode$field, 'userAgent', $elm$json$Json$Decode$string),
		flags);
	if (!_v0.$) {
		var userAgent = _v0.a;
		return $elm$core$Maybe$Just(userAgent);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Main$init = function (flags) {
	var userAgent = function () {
		var _v6 = $author$project$Main$userAgentFromFlags(flags);
		if (!_v6.$) {
			var decodedUserAgent = _v6.a;
			return decodedUserAgent;
		} else {
			return '';
		}
	}();
	var overrideOrdering = function () {
		var _v5 = $author$project$Main$overrideOrderingFromLocalStorage(flags);
		if (!_v5.$) {
			var overrideOrderingFlag = _v5.a;
			return overrideOrderingFlag;
		} else {
			return false;
		}
	}();
	var noTagsFilterActive = function () {
		var _v4 = $author$project$Main$noTagsFilterFromLocalStorage(flags);
		if (!_v4.$) {
			var filterActive = _v4.a;
			return filterActive;
		} else {
			return false;
		}
	}();
	var items = function () {
		var _v3 = $author$project$Main$itemsFromLocalStorage(flags);
		if (!_v3.$) {
			var itemDict = _v3.a;
			return itemDict;
		} else {
			return $elm$core$Dict$empty;
		}
	}();
	var filterTags = function () {
		var _v2 = $author$project$Main$filterTagsFromLocalStorage(flags);
		if (!_v2.$) {
			var filterTagList = _v2.a;
			return filterTagList;
		} else {
			return _List_Nil;
		}
	}();
	var clientId = function () {
		var _v1 = $author$project$Main$clientIdFromFlags(flags);
		if (!_v1.$) {
			var decodedClientId = _v1.a;
			return decodedClientId;
		} else {
			return '';
		}
	}();
	var apiKey = function () {
		var _v0 = $author$project$Main$apiKeyFromFlags(flags);
		if (!_v0.$) {
			var decodedApiKey = _v0.a;
			return decodedApiKey;
		} else {
			return '';
		}
	}();
	return _Utils_Tuple2(
		{G: apiKey, au: clientId, k: filterTags, aO: $elm$core$Maybe$Nothing, b: items, af: noTagsFilterActive, K: overrideOrdering, cC: userAgent},
		A3(
			$author$project$Main$syncItems,
			apiKey,
			clientId,
			A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.bp;
				},
				$elm$core$Dict$values(items))));
};
var $author$project$Main$GotFocus = function (a) {
	return {$: 25, a: a};
};
var $author$project$Main$ReceivedGeolocation = function (a) {
	return {$: 23, a: a};
};
var $author$project$Main$ReceivedMQTTMessageDeletedItem = function (a) {
	return {$: 16, a: a};
};
var $author$project$Main$ReceivedMQTTMessageDoneStatus = function (a) {
	return {$: 14, a: a};
};
var $author$project$Main$ReceivedMQTTMessageNewItem = function (a) {
	return {$: 15, a: a};
};
var $author$project$Main$ReceivedMQTTMessageUpdatedItem = function (a) {
	return {$: 17, a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $author$project$Main$gotFocus = _Platform_incomingPort('gotFocus', $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$receiveGeolocation = _Platform_incomingPort('receiveGeolocation', $elm$json$Json$Decode$value);
var $author$project$Main$receiveMQTTMessageDeletedItem = _Platform_incomingPort('receiveMQTTMessageDeletedItem', $elm$json$Json$Decode$string);
var $author$project$Main$receiveMQTTMessageDoneStatus = _Platform_incomingPort('receiveMQTTMessageDoneStatus', $elm$json$Json$Decode$string);
var $author$project$Main$receiveMQTTMessageNewItem = _Platform_incomingPort('receiveMQTTMessageNewItem', $elm$json$Json$Decode$string);
var $author$project$Main$receiveMQTTMessageUpdatedItem = _Platform_incomingPort('receiveMQTTMessageUpdatedItem', $elm$json$Json$Decode$string);
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$receiveMQTTMessageDoneStatus($author$project$Main$ReceivedMQTTMessageDoneStatus),
				$author$project$Main$receiveMQTTMessageNewItem($author$project$Main$ReceivedMQTTMessageNewItem),
				$author$project$Main$receiveMQTTMessageDeletedItem($author$project$Main$ReceivedMQTTMessageDeletedItem),
				$author$project$Main$receiveMQTTMessageUpdatedItem($author$project$Main$ReceivedMQTTMessageUpdatedItem),
				$author$project$Main$receiveGeolocation($author$project$Main$ReceivedGeolocation),
				$author$project$Main$gotFocus($author$project$Main$GotFocus)
			]));
};
var $author$project$Main$AddNewCard = function (a) {
	return {$: 7, a: a};
};
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$maxOrderIndex = function (items) {
	return $elm$core$List$maximum(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.bp;
			},
			items));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Main$addNewItem = F2(
	function (newId, dict) {
		var newIndex = function () {
			var _v0 = $author$project$ItemData$maxOrderIndex(
				$elm$core$Dict$values(dict));
			if (!_v0.$) {
				var maxIndex = _v0.a;
				return maxIndex + 1;
			} else {
				return 0;
			}
		}();
		return A3(
			$elm$core$Dict$insert,
			newId,
			{
				aI: $elm$core$Maybe$Just(-1),
				ad: 0,
				aj: false,
				aw: _List_Nil,
				ax: '',
				ay: '',
				aM: true,
				aA: newId,
				b1: -1,
				b6: true,
				aV: '',
				bp: newIndex,
				ch: newIndex,
				bA: false,
				_: _List_Nil,
				ag: ''
			},
			dict);
	});
var $author$project$ItemData$receivedToItem = F2(
	function (index, itemReceived) {
		return {
			aI: itemReceived.cO,
			ad: itemReceived.ad,
			aj: false,
			aw: itemReceived._,
			ax: '',
			ay: itemReceived.ag,
			aM: false,
			aA: itemReceived.aA,
			b1: itemReceived.dq,
			b6: false,
			aV: function () {
				var _v0 = itemReceived.aV;
				if (!_v0.$) {
					var oldId = _v0.a;
					return oldId;
				} else {
					return '';
				}
			}(),
			bp: index,
			ch: index,
			bA: true,
			_: itemReceived._,
			ag: itemReceived.ag
		};
	});
var $author$project$Main$addReceivedItem = F2(
	function (itemDataReceived, dict) {
		var newIndex = function () {
			var _v0 = $author$project$ItemData$maxOrderIndex(
				$elm$core$Dict$values(dict));
			if (!_v0.$) {
				var maxIndex = _v0.a;
				return maxIndex + 1;
			} else {
				return 0;
			}
		}();
		var itemData = A2($author$project$ItemData$receivedToItem, newIndex, itemDataReceived);
		return A3($elm$core$Dict$insert, itemData.aA, itemData, dict);
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Main$argsort = function (l) {
	return A2(
		$elm$core$List$map,
		$elm$core$Tuple$first,
		A2(
			$elm$core$List$sortBy,
			$elm$core$Tuple$second,
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, l)));
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Main$SortResponseReceived = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $author$project$Main$sortAPIResponseDecoder = A2(
	$elm$json$Json$Decode$field,
	'sort_indices',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$int));
var $author$project$Urls$dataBaseUrl = 'https://picluster.a-h.wtf/einkaufs_api';
var $author$project$Urls$sortUrl = $author$project$Urls$dataBaseUrl + '/sort/';
var $author$project$Main$callSortAPI = function (items) {
	return $elm$http$Http$post(
		{
			H: $elm$http$Http$jsonBody(
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'input_list',
							A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$string,
								A2(
									$elm$core$List$map,
									function ($) {
										return $.ag;
									},
									items)))
						]))),
			J: A2(
				$elm$http$Http$expectJson,
				$author$project$Main$SortResponseReceived(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.aA;
						},
						items)),
				$author$project$Main$sortAPIResponseDecoder),
			O: $author$project$Urls$sortUrl
		});
};
var $author$project$Main$DeleteResponseReceived = F2(
	function (a, b) {
		return {$: 13, a: a, b: b};
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Header = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$header = $elm$http$Http$Header;
var $author$project$Main$httpDelete = function (options) {
	return $elm$http$Http$request(
		{
			H: options.H,
			J: options.J,
			cZ: _List_fromArray(
				[
					A2($elm$http$Http$header, 'Content-Type', 'application/json')
				]),
			c6: 'DELETE',
			dE: $elm$core$Maybe$Nothing,
			dG: $elm$core$Maybe$Nothing,
			O: options.O
		});
};
var $author$project$Urls$itemUrl = F2(
	function (apiKey, itemId) {
		return $author$project$Urls$backendBaseUrl + ('/items/' + (itemId + ('?k=' + apiKey)));
	});
var $author$project$Main$deleteItem = F2(
	function (apiKey, itemId) {
		return $author$project$Main$httpDelete(
			{
				H: $elm$http$Http$emptyBody,
				J: A2(
					$elm$http$Http$expectJson,
					$author$project$Main$DeleteResponseReceived(itemId),
					A2($elm$json$Json$Decode$field, 'success', $elm$json$Json$Decode$bool)),
				O: A2($author$project$Urls$itemUrl, apiKey, itemId)
			});
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(0),
				dictionary));
	});
var $author$project$FilterTag$encode = function (_v0) {
	var tag = _v0.Z;
	var isActive = _v0.aQ;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'tag',
				$elm$json$Json$Encode$string(tag)),
				_Utils_Tuple2(
				'isActive',
				$elm$json$Json$Encode$bool(isActive))
			]));
};
var $author$project$Main$encodeModel = function (_v0) {
	var items = _v0.b;
	var overrideOrdering = _v0.K;
	var filterTags = _v0.k;
	var noTagsFilterActive = _v0.af;
	var apiKey = _v0.G;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'items',
				A3($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$ItemData$encode, items)),
				_Utils_Tuple2(
				'overrideOrdering',
				$elm$json$Json$Encode$bool(overrideOrdering)),
				_Utils_Tuple2(
				'filterTags',
				A2($elm$json$Json$Encode$list, $author$project$FilterTag$encode, filterTags)),
				_Utils_Tuple2(
				'noTagsFilterActive',
				$elm$json$Json$Encode$bool(noTagsFilterActive))
			]));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$ItemData$allTags = $elm$core$List$concatMap(
	function ($) {
		return $._;
	});
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$ItemData$uniqueTags = function (items) {
	return $elm$core$Set$fromList(
		$author$project$ItemData$allTags(items));
};
var $author$project$Main$filterTagNames = function (items) {
	return $elm$core$Set$toList(
		$author$project$ItemData$uniqueTags(
			$elm$core$Dict$values(items)));
};
var $author$project$FilterTag$fromTags = function (tagNames) {
	return A2(
		$elm$core$List$map,
		function (tag) {
			return A2($author$project$FilterTag$FilterTag, tag, false);
		},
		tagNames);
};
var $elm$random$Random$Generate = $elm$core$Basics$identity;
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = $elm$core$Basics$identity;
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0;
		return function (seed0) {
			var _v1 = genA(seed0);
			var a = _v1.a;
			var seed1 = _v1.b;
			return _Utils_Tuple2(
				func(a),
				seed1);
		};
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0;
		return A2($elm$random$Random$map, func, generator);
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			A2($elm$random$Random$map, tagger, generator));
	});
var $TSFoster$elm_uuid$UUID$UUID = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$random$Random$map4 = F5(
	function (func, _v0, _v1, _v2, _v3) {
		var genA = _v0;
		var genB = _v1;
		var genC = _v2;
		var genD = _v3;
		return function (seed0) {
			var _v4 = genA(seed0);
			var a = _v4.a;
			var seed1 = _v4.b;
			var _v5 = genB(seed1);
			var b = _v5.a;
			var seed2 = _v5.b;
			var _v6 = genC(seed2);
			var c = _v6.a;
			var seed3 = _v6.b;
			var _v7 = genD(seed3);
			var d = _v7.a;
			var seed4 = _v7.b;
			return _Utils_Tuple2(
				A4(func, a, b, c, d),
				seed4);
		};
	});
var $TSFoster$elm_uuid$UUID$forceUnsigned = $elm$core$Bitwise$shiftRightZfBy(0);
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return function (seed0) {
			var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
			var lo = _v0.a;
			var hi = _v0.b;
			var range = (hi - lo) + 1;
			if (!((range - 1) & range)) {
				return _Utils_Tuple2(
					(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
					$elm$random$Random$next(seed0));
			} else {
				var threshhold = (((-range) >>> 0) % range) >>> 0;
				var accountForBias = function (seed) {
					accountForBias:
					while (true) {
						var x = $elm$random$Random$peel(seed);
						var seedN = $elm$random$Random$next(seed);
						if (_Utils_cmp(x, threshhold) < 0) {
							var $temp$seed = seedN;
							seed = $temp$seed;
							continue accountForBias;
						} else {
							return _Utils_Tuple2((x % range) + lo, seedN);
						}
					}
				};
				return accountForBias(seed0);
			}
		};
	});
var $elm$random$Random$maxInt = 2147483647;
var $elm$random$Random$minInt = -2147483648;
var $TSFoster$elm_uuid$UUID$randomU32 = A2(
	$elm$random$Random$map,
	$TSFoster$elm_uuid$UUID$forceUnsigned,
	A2($elm$random$Random$int, $elm$random$Random$minInt, $elm$random$Random$maxInt));
var $elm$core$Bitwise$or = _Bitwise_or;
var $TSFoster$elm_uuid$UUID$toVariant1 = function (_v0) {
	var a = _v0.a;
	var b = _v0.b;
	var c = _v0.c;
	var d = _v0.d;
	return A4(
		$TSFoster$elm_uuid$UUID$UUID,
		a,
		b,
		$TSFoster$elm_uuid$UUID$forceUnsigned(2147483648 | (1073741823 & c)),
		d);
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $TSFoster$elm_uuid$UUID$toVersion = F2(
	function (v, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		var c = _v0.c;
		var d = _v0.d;
		return A4(
			$TSFoster$elm_uuid$UUID$UUID,
			a,
			$TSFoster$elm_uuid$UUID$forceUnsigned((v << 12) | (4294905855 & b)),
			c,
			d);
	});
var $TSFoster$elm_uuid$UUID$generator = A2(
	$elm$random$Random$map,
	A2(
		$elm$core$Basics$composeR,
		$TSFoster$elm_uuid$UUID$toVersion(4),
		$TSFoster$elm_uuid$UUID$toVariant1),
	A5($elm$random$Random$map4, $TSFoster$elm_uuid$UUID$UUID, $TSFoster$elm_uuid$UUID$randomU32, $TSFoster$elm_uuid$UUID$randomU32, $TSFoster$elm_uuid$UUID$randomU32, $TSFoster$elm_uuid$UUID$randomU32));
var $author$project$ItemData$incrementMaybe = function (maybeInt) {
	if (!maybeInt.$) {
		var _int = maybeInt.a;
		return $elm$core$Maybe$Just(_int + 1);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$incrementClientRevision = function (maybeItem) {
	if (!maybeItem.$) {
		var item = maybeItem.a;
		return $elm$core$Maybe$Just(
			_Utils_update(
				item,
				{
					aI: $author$project$ItemData$incrementMaybe(item.aI)
				}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$itemListToAssoc = function (items) {
	return A2(
		$elm$core$List$map,
		function (item) {
			return _Utils_Tuple2(item.aA, item);
		},
		items);
};
var $author$project$ItemData$itemListToDict = function (items) {
	return $elm$core$Dict$fromList(
		$author$project$ItemData$itemListToAssoc(items));
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$FilterTag$merge = F2(
	function (oldTags, newTags) {
		var oldNames = A2(
			$elm$core$List$map,
			function ($) {
				return $.Z;
			},
			oldTags);
		var newNames = A2(
			$elm$core$List$map,
			function ($) {
				return $.Z;
			},
			newTags);
		var tagsToKeep = A2(
			$elm$core$List$filter,
			function (tag) {
				return A2($elm$core$List$member, tag.Z, newNames);
			},
			oldTags);
		var additionalTags = A2(
			$elm$core$List$filter,
			function (tag) {
				return !A2($elm$core$List$member, tag.Z, oldNames);
			},
			newTags);
		return _Utils_ap(tagsToKeep, additionalTags);
	});
var $author$project$Main$clientRevisionsMismatch = F2(
	function (valLeft, valRight) {
		var _v0 = valLeft.aI;
		if (!_v0.$) {
			var requestClientRevision = _v0.a;
			var _v1 = valRight.aI;
			if (!_v1.$) {
				var currentClientRevision = _v1.a;
				return (_Utils_cmp(currentClientRevision, requestClientRevision) > 0) ? true : false;
			} else {
				return false;
			}
		} else {
			return false;
		}
	});
var $author$project$Main$both = F4(
	function (key, valLeft, valRight, res) {
		var keepClientItem = A2($author$project$Main$clientRevisionsMismatch, valLeft, valRight);
		return valRight.aM ? A3($elm$core$Dict$insert, key, valRight, res) : (keepClientItem ? A3($elm$core$Dict$insert, key, valRight, res) : A3(
			$elm$core$Dict$insert,
			key,
			_Utils_update(
				valRight,
				{
					aI: $elm$core$Maybe$Just(valLeft.b1),
					ad: valLeft.ad,
					b1: valLeft.b1,
					bp: valLeft.bp,
					bA: true,
					_: valLeft._,
					ag: valLeft.ag
				}),
			res));
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $author$project$Main$newOnly = F4(
	function (oldDict, key, val, res) {
		var _v0 = A2($elm$core$Dict$get, val.aV, oldDict);
		if (!_v0.$) {
			var oldItem = _v0.a;
			var keepClientItem = A2($author$project$Main$clientRevisionsMismatch, val, oldItem);
			return keepClientItem ? A3($elm$core$Dict$insert, key, oldItem, res) : A3(
				$elm$core$Dict$insert,
				key,
				_Utils_update(
					oldItem,
					{
						aI: $elm$core$Maybe$Just(val.b1),
						ad: val.ad,
						aA: key,
						b1: val.b1,
						aV: oldItem.aA,
						bp: val.bp,
						bA: true,
						_: val._,
						ag: val.ag
					}),
				res);
		} else {
			return A3($elm$core$Dict$insert, key, val, res);
		}
	});
var $author$project$Main$oldOnly = F3(
	function (key, val, res) {
		return res;
	});
var $author$project$Main$mergeIntoItemDict = F2(
	function (newDict, oldDict) {
		return A6(
			$elm$core$Dict$merge,
			$author$project$Main$newOnly(oldDict),
			$author$project$Main$both,
			$author$project$Main$oldOnly,
			newDict,
			oldDict,
			$elm$core$Dict$empty);
	});
var $author$project$Main$hasMismatchedClientRevision = function (item) {
	var _v0 = item.aI;
	if (!_v0.$) {
		var clientRevision = _v0.a;
		return _Utils_cmp(clientRevision, item.b1) > 0;
	} else {
		return true;
	}
};
var $author$project$Main$mismatchedClientRevisions = function (items) {
	return A3(
		$elm$core$List$foldl,
		$elm$core$Basics$or,
		false,
		A2(
			$elm$core$List$map,
			$author$project$Main$hasMismatchedClientRevision,
			$elm$core$Dict$values(items)));
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$Geolocation = F2(
	function (latitude, longitude) {
		return {c2: latitude, c5: longitude};
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Main$parseGeolocation = function (portMsg) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		A3(
			$elm$json$Json$Decode$map2,
			$author$project$Main$Geolocation,
			A2($elm$json$Json$Decode$field, 'latitude', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'longitude', $elm$json$Json$Decode$float)),
		portMsg);
	if (!_v0.$) {
		var geolocation = _v0.a;
		return $elm$core$Maybe$Just(geolocation);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$parseMQTTClientId = function (rawString) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeString,
		A2($elm$json$Json$Decode$field, 'clientId', $elm$json$Json$Decode$string),
		rawString);
	if (!_v0.$) {
		var clientId = _v0.a;
		return $elm$core$Maybe$Just(clientId);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$MqttMessageDoneStatus = F2(
	function (id, status) {
		return {aA: id, dz: status};
	});
var $author$project$Main$parseMQTTMessageDoneStatus = function (rawString) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeString,
		A3(
			$elm$json$Json$Decode$map2,
			$author$project$Main$MqttMessageDoneStatus,
			A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$int)),
		rawString);
	if (!_v0.$) {
		var mqttData = _v0.a;
		return $elm$core$Maybe$Just(mqttData);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$parseMQTTMessageItemDeleted = function (rawString) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeString,
		A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
		rawString);
	if (!_v0.$) {
		var itemId = _v0.a;
		return $elm$core$Maybe$Just(itemId);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$ItemDataReceived = F7(
	function (id, title, tags, done, revision, clientRevisionWas, oldId) {
		return {cO: clientRevisionWas, ad: done, aA: id, aV: oldId, dq: revision, _: tags, ag: title};
	});
var $elm$json$Json$Decode$map7 = _Json_map7;
var $author$project$ItemData$jsonParseItemDataReceived = A8(
	$elm$json$Json$Decode$map7,
	$author$project$ItemData$ItemDataReceived,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'tags',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'done', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'revision',
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					$elm$json$Json$Decode$int,
					$elm$json$Json$Decode$null(0)
				]))),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$field, 'clientRevisionWas', $elm$json$Json$Decode$int)),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$field, 'oldId', $elm$json$Json$Decode$string)));
var $author$project$Main$parseMQTTMessageNewItem = function (rawString) {
	var _v0 = A2($elm$json$Json$Decode$decodeString, $author$project$ItemData$jsonParseItemDataReceived, rawString);
	if (!_v0.$) {
		var itemDataReceived = _v0.a;
		return $elm$core$Maybe$Just(itemDataReceived);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$jsonParseReceivedItemList = $elm$json$Json$Decode$list($author$project$ItemData$jsonParseItemDataReceived);
var $author$project$ItemData$parseReceivedItems = function (rawString) {
	var _v0 = A2($elm$json$Json$Decode$decodeString, $author$project$ItemData$jsonParseReceivedItemList, rawString);
	if (!_v0.$) {
		var itemsList = _v0.a;
		return itemsList;
	} else {
		return _List_Nil;
	}
};
var $author$project$Main$CollectResponseReceived = function (a) {
	return {$: 24, a: a};
};
var $author$project$Urls$collectUrl = $author$project$Urls$dataBaseUrl + '/collect/';
var $elm$http$Http$expectBytesResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'arraybuffer',
			_Http_toDataView,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$expectWhatever = function (toMsg) {
	return A2(
		$elm$http$Http$expectBytesResponse,
		toMsg,
		$elm$http$Http$resolve(
			function (_v0) {
				return $elm$core$Result$Ok(0);
			}));
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$Main$postCollectEvent = F2(
	function (model, item) {
		var longitudeValue = function () {
			var _v2 = model.aO;
			if (!_v2.$) {
				var geolocation = _v2.a;
				return $elm$json$Json$Encode$float(geolocation.c5);
			} else {
				return $elm$json$Json$Encode$null;
			}
		}();
		var latitudeValue = function () {
			var _v1 = model.aO;
			if (!_v1.$) {
				var geolocation = _v1.a;
				return $elm$json$Json$Encode$float(geolocation.c2);
			} else {
				return $elm$json$Json$Encode$null;
			}
		}();
		var actionType = function () {
			var _v0 = item.ad;
			if (!_v0) {
				return 'UNCROSSED';
			} else {
				return 'CROSSED';
			}
		}();
		return $elm$http$Http$post(
			{
				H: $elm$http$Http$jsonBody(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'action_type',
								$elm$json$Json$Encode$string(actionType)),
								_Utils_Tuple2(
								'name',
								$elm$json$Json$Encode$string(item.ag)),
								_Utils_Tuple2(
								'item_id',
								$elm$json$Json$Encode$string(item.aA)),
								_Utils_Tuple2('latitude', latitudeValue),
								_Utils_Tuple2('longitude', longitudeValue),
								_Utils_Tuple2(
								'user_agent',
								$elm$json$Json$Encode$string(model.cC)),
								_Utils_Tuple2(
								'user_key',
								$elm$json$Json$Encode$string(model.G))
							]))),
				J: $elm$http$Http$expectWhatever($author$project$Main$CollectResponseReceived),
				O: $author$project$Urls$collectUrl
			});
	});
var $author$project$Main$ItemPosted = F2(
	function (a, b) {
		return {$: 18, a: a, b: b};
	});
var $author$project$Main$PostResponse = F3(
	function (success, newId, revision) {
		return {b7: newId, dq: revision, dC: success};
	});
var $author$project$Urls$itemsUrl = F2(
	function (apiKey, clientId) {
		return $author$project$Urls$backendBaseUrl + ('/items?k=' + (apiKey + ('&c=' + clientId)));
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Main$postItem = F3(
	function (apiKey, clientId, item) {
		return $elm$http$Http$post(
			{
				H: $elm$http$Http$jsonBody(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'itemData',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'title',
											$elm$json$Json$Encode$string(item.ag)),
											_Utils_Tuple2(
											'tags',
											A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, item._))
										])))
							]))),
				J: A2(
					$elm$http$Http$expectJson,
					$author$project$Main$ItemPosted(item.aA),
					A4(
						$elm$json$Json$Decode$map3,
						$author$project$Main$PostResponse,
						A2($elm$json$Json$Decode$field, 'success', $elm$json$Json$Decode$bool),
						A2($elm$json$Json$Decode$field, 'newId', $elm$json$Json$Decode$string),
						A2($elm$json$Json$Decode$field, 'revision', $elm$json$Json$Decode$int))),
				O: A2($author$project$Urls$itemsUrl, apiKey, clientId)
			});
	});
var $author$project$Main$NoOp = {$: 27};
var $elm$browser$Browser$Dom$setViewport = _Browser_setViewport;
var $author$project$Main$resetViewport = A2(
	$elm$core$Task$perform,
	function (_v0) {
		return $author$project$Main$NoOp;
	},
	A2($elm$browser$Browser$Dom$setViewport, 0, 0));
var $author$project$ItemData$setDone = F2(
	function (newStatus, maybeItem) {
		if (!maybeItem.$) {
			var item = maybeItem.a;
			return $elm$core$Maybe$Just(
				_Utils_update(
					item,
					{ad: newStatus}));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$ItemData$setSynced = F2(
	function (newSynced, maybeItem) {
		if (!maybeItem.$) {
			var item = maybeItem.a;
			return $elm$core$Maybe$Just(
				_Utils_update(
					item,
					{bA: newSynced}));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$String$fromList = _String_fromList;
var $TSFoster$elm_uuid$UUID$toHex = F2(
	function (acc, _int) {
		toHex:
		while (true) {
			if (!_int) {
				return $elm$core$String$fromList(acc);
			} else {
				var _char = function () {
					var _v0 = 15 & _int;
					switch (_v0) {
						case 0:
							return '0';
						case 1:
							return '1';
						case 2:
							return '2';
						case 3:
							return '3';
						case 4:
							return '4';
						case 5:
							return '5';
						case 6:
							return '6';
						case 7:
							return '7';
						case 8:
							return '8';
						case 9:
							return '9';
						case 10:
							return 'a';
						case 11:
							return 'b';
						case 12:
							return 'c';
						case 13:
							return 'd';
						case 14:
							return 'e';
						default:
							return 'f';
					}
				}();
				var $temp$acc = A2($elm$core$List$cons, _char, acc),
					$temp$int = _int >>> 4;
				acc = $temp$acc;
				_int = $temp$int;
				continue toHex;
			}
		}
	});
var $TSFoster$elm_uuid$UUID$toStringWith = F2(
	function (sep, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		var c = _v0.c;
		var d = _v0.d;
		return _Utils_ap(
			A3(
				$elm$core$String$padLeft,
				8,
				'0',
				A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, a)),
			_Utils_ap(
				sep,
				_Utils_ap(
					A3(
						$elm$core$String$padLeft,
						4,
						'0',
						A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, b >>> 16)),
					_Utils_ap(
						sep,
						_Utils_ap(
							A3(
								$elm$core$String$padLeft,
								4,
								'0',
								A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, 65535 & b)),
							_Utils_ap(
								sep,
								_Utils_ap(
									A3(
										$elm$core$String$padLeft,
										4,
										'0',
										A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, c >>> 16)),
									_Utils_ap(
										sep,
										_Utils_ap(
											A3(
												$elm$core$String$padLeft,
												4,
												'0',
												A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, 65535 & c)),
											A3(
												$elm$core$String$padLeft,
												8,
												'0',
												A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, d)))))))))));
	});
var $TSFoster$elm_uuid$UUID$toString = $TSFoster$elm_uuid$UUID$toStringWith('-');
var $author$project$FilterTag$toggleByTag = F2(
	function (tag, tagList) {
		var toggle = F2(
			function (tagToMatch, filterTag) {
				return _Utils_eq(tagToMatch, filterTag.Z) ? _Utils_update(
					filterTag,
					{aQ: !filterTag.aQ}) : filterTag;
			});
		return A2(
			$elm$core$List$map,
			toggle(tag),
			tagList);
	});
var $author$project$ItemData$toggleDone = function (maybeItem) {
	if (!maybeItem.$) {
		var item = maybeItem.a;
		return $elm$core$Maybe$Just(
			_Utils_update(
				item,
				{
					ad: (!item.ad) ? 1 : 0
				}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ItemData$toggleEdit = function (maybeItem) {
	if (!maybeItem.$) {
		var item = maybeItem.a;
		return $elm$core$Maybe$Just(
			_Utils_update(
				item,
				{aj: false, aw: item._, ax: '', ay: item.ag, aM: !item.aM}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$trim = _String_trim;
var $author$project$Main$DoneResponseReceived = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var $author$project$Main$httpUpdate = function (options) {
	return $elm$http$Http$request(
		{
			H: options.H,
			J: options.J,
			cZ: _List_fromArray(
				[
					A2($elm$http$Http$header, 'Content-Type', 'application/json')
				]),
			c6: 'UPDATE',
			dE: $elm$core$Maybe$Nothing,
			dG: $elm$core$Maybe$Nothing,
			O: options.O
		});
};
var $author$project$Urls$updateDoneUrl = F2(
	function (apiKey, itemId) {
		return $author$project$Urls$backendBaseUrl + ('/items/' + (itemId + ('/done?k=' + apiKey)));
	});
var $author$project$Main$updateDoneBackend = F3(
	function (apiKey, itemId, doneStatus) {
		return $author$project$Main$httpUpdate(
			{
				H: $elm$http$Http$jsonBody(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'done',
								$elm$json$Json$Encode$int(doneStatus))
							]))),
				J: A2(
					$elm$http$Http$expectJson,
					$author$project$Main$DoneResponseReceived(itemId),
					A2($elm$json$Json$Decode$field, 'success', $elm$json$Json$Decode$bool)),
				O: A2($author$project$Urls$updateDoneUrl, apiKey, itemId)
			});
	});
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$ItemData$listEqual = F2(
	function (listA, listB) {
		var sortedB = $elm$core$List$sort(listB);
		var sortedA = $elm$core$List$sort(listA);
		return A3(
			$elm$core$List$foldr,
			$elm$core$Basics$and,
			_Utils_eq(
				$elm$core$List$length(listA),
				$elm$core$List$length(listB)),
			A2(
				$elm$core$List$map,
				function (x) {
					return _Utils_eq(x.a, x.b);
				},
				A3($elm$core$List$map2, $elm$core$Tuple$pair, sortedA, sortedB)));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$ItemData$draftHasChanged = function (item) {
	return (!_Utils_eq(item.ay, item.ag)) || ((!A2($author$project$ItemData$listEqual, item.aw, item._)) || ($elm$core$String$trim(item.ax) !== ''));
};
var $author$project$ItemData$updateDraftTags = F2(
	function (newDraftTags, maybeItem) {
		if (!maybeItem.$) {
			var item = maybeItem.a;
			var newItem = _Utils_update(
				item,
				{aw: newDraftTags});
			return $elm$core$Maybe$Just(
				_Utils_update(
					newItem,
					{
						aj: $author$project$ItemData$draftHasChanged(newItem)
					}));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$ItemData$updateDraftTagsInput = F2(
	function (newText, maybeItem) {
		if (!maybeItem.$) {
			var item = maybeItem.a;
			var newItem = _Utils_update(
				item,
				{ax: newText});
			return $elm$core$Maybe$Just(
				_Utils_update(
					newItem,
					{
						aj: $author$project$ItemData$draftHasChanged(newItem)
					}));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$ItemData$updateDraftTitle = F2(
	function (newTitle, maybeItem) {
		if (!maybeItem.$) {
			var item = maybeItem.a;
			var newItem = _Utils_update(
				item,
				{ay: newTitle});
			return $elm$core$Maybe$Just(
				_Utils_update(
					newItem,
					{
						aj: $author$project$ItemData$draftHasChanged(newItem)
					}));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$updateFromReceivedItem = F2(
	function (itemDataReceived, dict) {
		return A3(
			$elm$core$Dict$update,
			itemDataReceived.aA,
			function (maybeItem) {
				if (!maybeItem.$) {
					var item = maybeItem.a;
					return item.aM ? $elm$core$Maybe$Just(item) : $elm$core$Maybe$Just(
						_Utils_update(
							item,
							{ad: itemDataReceived.ad, _: itemDataReceived._, ag: itemDataReceived.ag}));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			dict);
	});
var $author$project$Main$UpdateResponse = F2(
	function (success, revision) {
		return {dq: revision, dC: success};
	});
var $author$project$Main$UpdateResponseReceived = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $author$project$Main$updateItem = F2(
	function (apiKey, item) {
		return $author$project$Main$httpUpdate(
			{
				H: $elm$http$Http$jsonBody(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'itemData',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'title',
											$elm$json$Json$Encode$string(item.ag)),
											_Utils_Tuple2(
											'tags',
											A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, item._))
										])))
							]))),
				J: A2(
					$elm$http$Http$expectJson,
					$author$project$Main$UpdateResponseReceived(item.aA),
					A3(
						$elm$json$Json$Decode$map2,
						$author$project$Main$UpdateResponse,
						A2($elm$json$Json$Decode$field, 'success', $elm$json$Json$Decode$bool),
						A2($elm$json$Json$Decode$field, 'revision', $elm$json$Json$Decode$int))),
				O: A2($author$project$Urls$itemUrl, apiKey, item.aA)
			});
	});
var $author$project$Main$updateOverrideOrderIndex = F3(
	function (idToIndexDict, itemId, item) {
		return _Utils_update(
			item,
			{
				ch: function () {
					var _v0 = A2($elm$core$Dict$get, itemId, idToIndexDict);
					if (!_v0.$) {
						var newIndex = _v0.a;
						return newIndex;
					} else {
						return item.ch;
					}
				}()
			});
	});
var $author$project$Main$writeToLocalStorage = _Platform_outgoingPort('writeToLocalStorage', $elm$core$Basics$identity);
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var payload = msg.a;
				if (!payload.$) {
					var rawString = payload.a;
					var serverItems = $author$project$ItemData$itemListToDict(
						A2(
							$elm$core$List$indexedMap,
							$author$project$ItemData$receivedToItem,
							$author$project$ItemData$parseReceivedItems(rawString)));
					var items = A2($author$project$Main$mergeIntoItemDict, serverItems, model.b);
					var newFilterTags = A2(
						$author$project$FilterTag$merge,
						model.k,
						$author$project$FilterTag$fromTags(
							$author$project$Main$filterTagNames(items)));
					var newModel = _Utils_update(
						model,
						{k: newFilterTags, b: items});
					return _Utils_Tuple2(
						newModel,
						$elm$core$Platform$Cmd$batch(
							_Utils_ap(
								model.K ? _List_fromArray(
									[
										$author$project$Main$callSortAPI(
										$elm$core$Dict$values(items))
									]) : _List_Nil,
								_Utils_ap(
									$author$project$Main$mismatchedClientRevisions(items) ? _List_fromArray(
										[
											A3(
											$author$project$Main$syncItems,
											model.G,
											model.au,
											A2(
												$elm$core$List$sortBy,
												function ($) {
													return $.bp;
												},
												$elm$core$Dict$values(items)))
										]) : _List_Nil,
									_List_fromArray(
										[
											$author$project$Main$writeToLocalStorage(
											$author$project$Main$encodeModel(newModel))
										])))));
				} else {
					var httpError = payload.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 2:
				var tag = msg.a;
				var newModel = _Utils_update(
					model,
					{
						k: A2($author$project$FilterTag$toggleByTag, tag, model.k)
					});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 3:
				var itemId = msg.a;
				var newItems = A3(
					$elm$core$Dict$update,
					itemId,
					function (item) {
						return $author$project$ItemData$incrementClientRevision(
							A2(
								$author$project$ItemData$setSynced,
								false,
								$author$project$ItemData$toggleDone(item)));
					},
					model.b);
				var newModel = _Utils_update(
					model,
					{b: newItems});
				var maybeItem = A2($elm$core$Dict$get, itemId, newItems);
				return _Utils_Tuple2(
					newModel,
					$elm$core$Platform$Cmd$batch(
						_Utils_ap(
							function () {
								if (!maybeItem.$) {
									var item = maybeItem.a;
									return _List_fromArray(
										[
											A3($author$project$Main$updateDoneBackend, model.G, itemId, item.ad),
											A2($author$project$Main$postCollectEvent, model, item)
										]);
								} else {
									return _List_Nil;
								}
							}(),
							_List_fromArray(
								[
									$author$project$Main$writeToLocalStorage(
									$author$project$Main$encodeModel(newModel))
								]))));
			case 1:
				var itemId = msg.a;
				var newModel = _Utils_update(
					model,
					{
						b: A3($elm$core$Dict$update, itemId, $author$project$ItemData$toggleEdit, model.b)
					});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 4:
				var itemId = msg.a;
				var maybeItem = A2($elm$core$Dict$get, itemId, model.b);
				if (!maybeItem.$) {
					var item = maybeItem.a;
					var newModel = item.b6 ? _Utils_update(
						model,
						{
							b: A2($elm$core$Dict$remove, itemId, model.b)
						}) : _Utils_update(
						model,
						{
							b: A3($elm$core$Dict$update, itemId, $author$project$ItemData$toggleEdit, model.b)
						});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 5:
				var itemId = msg.a;
				var _v4 = A2($elm$core$Dict$get, itemId, model.b);
				if (!_v4.$) {
					var item = _v4.a;
					if (!item.aj) {
						var newModel = _Utils_update(
							model,
							{
								b: A3($elm$core$Dict$update, itemId, $author$project$ItemData$toggleEdit, model.b)
							});
						return _Utils_Tuple2(
							newModel,
							$author$project$Main$writeToLocalStorage(
								$author$project$Main$encodeModel(newModel)));
					} else {
						var remainingText = $elm$core$String$trim(item.ax);
						var updatedItem = _Utils_update(
							item,
							{
								aI: $author$project$ItemData$incrementMaybe(item.aI),
								ax: '',
								aM: false,
								b6: false,
								bA: false,
								_: _Utils_ap(
									item.aw,
									(remainingText === '') ? _List_Nil : _List_fromArray(
										[remainingText])),
								ag: $elm$core$String$trim(item.ay)
							});
						var newItems = A3(
							$elm$core$Dict$update,
							itemId,
							function (i) {
								if (!i.$) {
									var it = i.a;
									return $elm$core$Maybe$Just(updatedItem);
								} else {
									return $elm$core$Maybe$Nothing;
								}
							},
							model.b);
						var newFilters = A2(
							$author$project$FilterTag$merge,
							model.k,
							$author$project$FilterTag$fromTags(
								$author$project$Main$filterTagNames(newItems)));
						var newModel = _Utils_update(
							model,
							{k: newFilters, b: newItems});
						return _Utils_Tuple2(
							newModel,
							$elm$core$Platform$Cmd$batch(
								A2(
									$elm$core$List$cons,
									item.b6 ? A3($author$project$Main$postItem, model.G, model.au, updatedItem) : A2($author$project$Main$updateItem, model.G, updatedItem),
									A2(
										$elm$core$List$cons,
										model.K ? $author$project$Main$callSortAPI(
											$elm$core$Dict$values(newItems)) : $elm$core$Platform$Cmd$none,
										_List_fromArray(
											[
												$author$project$Main$writeToLocalStorage(
												$author$project$Main$encodeModel(newModel))
											])))));
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 6:
				return _Utils_Tuple2(
					model,
					A2($elm$random$Random$generate, $author$project$Main$AddNewCard, $TSFoster$elm_uuid$UUID$generator));
			case 7:
				var newUUID = msg.a;
				var newId = 'local-' + $TSFoster$elm_uuid$UUID$toString(newUUID);
				var newModel = _Utils_update(
					model,
					{
						b: A2($author$project$Main$addNewItem, newId, model.b)
					});
				return _Utils_Tuple2(
					newModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$resetViewport,
								$author$project$Main$writeToLocalStorage(
								$author$project$Main$encodeModel(newModel))
							])));
			case 8:
				var itemId = msg.a;
				var newTitle = msg.b;
				var newModel = _Utils_update(
					model,
					{
						b: A3(
							$elm$core$Dict$update,
							itemId,
							$author$project$ItemData$updateDraftTitle(newTitle),
							model.b)
					});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 9:
				if (model.K) {
					var newModel = _Utils_update(
						model,
						{K: false});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					return _Utils_Tuple2(
						model,
						$author$project$Main$callSortAPI(
							$elm$core$Dict$values(model.b)));
				}
			case 10:
				var requestedIds = msg.a;
				var payload = msg.b;
				if (!payload.$) {
					var sortIndices = payload.a;
					var newIndices = $author$project$Main$argsort(
						$elm$core$List$reverse(sortIndices));
					var idToIndexDict = $elm$core$Dict$fromList(
						A3($elm$core$List$map2, $elm$core$Tuple$pair, requestedIds, newIndices));
					var newModel = _Utils_update(
						model,
						{
							b: A2(
								$elm$core$Dict$map,
								$author$project$Main$updateOverrideOrderIndex(idToIndexDict),
								model.b),
							K: true
						});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					var httpError = payload.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 11:
				var itemId = msg.a;
				var success = msg.b;
				var newModel = function () {
					if (!success.$) {
						var successValue = success.a;
						return successValue ? _Utils_update(
							model,
							{
								b: A3(
									$elm$core$Dict$update,
									itemId,
									$author$project$ItemData$setSynced(true),
									model.b)
							}) : model;
					} else {
						var httpError = success.a;
						return model;
					}
				}();
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 12:
				var itemId = msg.a;
				var updatePayload = msg.b;
				if (!updatePayload.$) {
					var updateResponse = updatePayload.a;
					var newItemDict = A3(
						$elm$core$Dict$update,
						itemId,
						function (maybeItem) {
							if (!maybeItem.$) {
								var item = maybeItem.a;
								return $elm$core$Maybe$Just(
									_Utils_update(
										item,
										{b1: updateResponse.dq, bA: true}));
							} else {
								return $elm$core$Maybe$Nothing;
							}
						},
						model.b);
					var newModel = _Utils_update(
						model,
						{b: newItemDict});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					var httpError = updatePayload.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 22:
				var doneIds = A2(
					$elm$core$List$map,
					function ($) {
						return $.aA;
					},
					A2(
						$elm$core$List$filter,
						function (item) {
							return item.ad === 1;
						},
						$elm$core$Dict$values(model.b)));
				return _Utils_Tuple2(
					model,
					$elm$core$Platform$Cmd$batch(
						A2(
							$elm$core$List$map,
							$author$project$Main$deleteItem(model.G),
							doneIds)));
			case 13:
				var itemId = msg.a;
				var successPayload = msg.b;
				if (!successPayload.$) {
					var success = successPayload.a;
					if (success) {
						var newItems = A2($elm$core$Dict$remove, itemId, model.b);
						var newFilters = A2(
							$author$project$FilterTag$merge,
							model.k,
							$author$project$FilterTag$fromTags(
								$author$project$Main$filterTagNames(newItems)));
						var newModel = _Utils_update(
							model,
							{k: newFilters, b: newItems});
						return _Utils_Tuple2(
							newModel,
							$author$project$Main$writeToLocalStorage(
								$author$project$Main$encodeModel(newModel)));
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				} else {
					var httpError = successPayload.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 14:
				var mqttMsg = msg.a;
				var maybeMqttData = $author$project$Main$parseMQTTMessageDoneStatus(mqttMsg);
				if (!maybeMqttData.$) {
					var mqttData = maybeMqttData.a;
					var newModel = _Utils_update(
						model,
						{
							b: A3(
								$elm$core$Dict$update,
								mqttData.aA,
								$author$project$ItemData$setDone(mqttData.dz),
								model.b)
						});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 15:
				var mqttMsg = msg.a;
				var maybeReceivedItem = $author$project$Main$parseMQTTMessageNewItem(mqttMsg);
				var maybeClientId = $author$project$Main$parseMQTTClientId(mqttMsg);
				var processMessage = function () {
					if (!maybeClientId.$) {
						var clientId = maybeClientId.a;
						return _Utils_eq(clientId, model.au) ? false : true;
					} else {
						return true;
					}
				}();
				var newItems = function () {
					if (processMessage) {
						if (!maybeReceivedItem.$) {
							var receivedItem = maybeReceivedItem.a;
							return A2($author$project$Main$addReceivedItem, receivedItem, model.b);
						} else {
							return model.b;
						}
					} else {
						return model.b;
					}
				}();
				var newModel = _Utils_update(
					model,
					{b: newItems});
				return _Utils_Tuple2(
					newModel,
					$elm$core$Platform$Cmd$batch(
						A2(
							$elm$core$List$cons,
							model.K ? $author$project$Main$callSortAPI(
								$elm$core$Dict$values(newItems)) : $elm$core$Platform$Cmd$none,
							_List_fromArray(
								[
									$author$project$Main$writeToLocalStorage(
									$author$project$Main$encodeModel(newModel))
								]))));
			case 18:
				var oldId = msg.a;
				var postResponsePayload = msg.b;
				if (!postResponsePayload.$) {
					var postResponse = postResponsePayload.a;
					var maybeOldItem = A2($elm$core$Dict$get, oldId, model.b);
					var newItems = function () {
						if (!maybeOldItem.$) {
							var oldItem = maybeOldItem.a;
							return A3(
								$elm$core$Dict$insert,
								postResponse.b7,
								_Utils_update(
									oldItem,
									{aA: postResponse.b7, b1: postResponse.dq, bA: true}),
								A2($elm$core$Dict$remove, oldId, model.b));
						} else {
							return model.b;
						}
					}();
					var newModel = _Utils_update(
						model,
						{b: newItems});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					var httpError = postResponsePayload.a;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 19:
				var itemId = msg.a;
				var newTagList = msg.b;
				var newModel = _Utils_update(
					model,
					{
						b: A3(
							$elm$core$Dict$update,
							itemId,
							$author$project$ItemData$updateDraftTags(newTagList),
							model.b)
					});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 20:
				var itemId = msg.a;
				var remainingText = msg.b;
				var newModel = _Utils_update(
					model,
					{
						b: A3(
							$elm$core$Dict$update,
							itemId,
							$author$project$ItemData$updateDraftTagsInput(remainingText),
							model.b)
					});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 21:
				var itemId = msg.a;
				return _Utils_Tuple2(
					model,
					A2($author$project$Main$deleteItem, model.G, itemId));
			case 16:
				var mqttMsg = msg.a;
				var maybeItemId = $author$project$Main$parseMQTTMessageItemDeleted(mqttMsg);
				var newItems = function () {
					if (!maybeItemId.$) {
						var itemId = maybeItemId.a;
						return A2($elm$core$Dict$remove, itemId, model.b);
					} else {
						return model.b;
					}
				}();
				var newFilters = A2(
					$author$project$FilterTag$merge,
					model.k,
					$author$project$FilterTag$fromTags(
						$author$project$Main$filterTagNames(newItems)));
				var newModel = _Utils_update(
					model,
					{k: newFilters, b: newItems});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 17:
				var mqttMsg = msg.a;
				var maybeUpdatedItem = $author$project$Main$parseMQTTMessageNewItem(mqttMsg);
				var newItems = function () {
					if (!maybeUpdatedItem.$) {
						var updatedItem = maybeUpdatedItem.a;
						return A2($author$project$Main$updateFromReceivedItem, updatedItem, model.b);
					} else {
						return model.b;
					}
				}();
				var newFilters = A2(
					$author$project$FilterTag$merge,
					model.k,
					$author$project$FilterTag$fromTags(
						$author$project$Main$filterTagNames(newItems)));
				var newModel = _Utils_update(
					model,
					{k: newFilters, b: newItems});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			case 23:
				var portMsg = msg.a;
				var _v18 = $author$project$Main$parseGeolocation(portMsg);
				if (!_v18.$) {
					var geolocation = _v18.a;
					var newModel = _Utils_update(
						model,
						{
							aO: $elm$core$Maybe$Just(geolocation)
						});
					return _Utils_Tuple2(
						newModel,
						$author$project$Main$writeToLocalStorage(
							$author$project$Main$encodeModel(newModel)));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 24:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 25:
				return _Utils_Tuple2(
					model,
					A3(
						$author$project$Main$syncItems,
						model.G,
						model.au,
						A2(
							$elm$core$List$sortBy,
							function ($) {
								return $.bp;
							},
							$elm$core$Dict$values(model.b))));
			case 26:
				var newModel = _Utils_update(
					model,
					{af: !model.af});
				return _Utils_Tuple2(
					newModel,
					$author$project$Main$writeToLocalStorage(
						$author$project$Main$encodeModel(newModel)));
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$AddNewCardClicked = {$: 6};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel = $elm$html$Html$Attributes$attribute('aria-label');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$i = _VirtualDom_node('i');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $fapian$elm_html_aria$Html$Attributes$Aria$role = $elm$html$Html$Attributes$attribute('role');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$addCardButton = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('fixed-action-btn center-horizontally')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$a,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn-floating btn-large waves-effect red'),
					$fapian$elm_html_aria$Html$Attributes$Aria$role('button'),
					$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Neuer Eintrag'),
					$elm$html$Html$Events$onClick($author$project$Main$AddNewCardClicked)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$i,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('large material-icons')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('add')
						]))
				]))
		]));
var $author$project$Main$DeleteAllDone = {$: 22};
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Main$delAllModal = A3(
	$elm$html$Html$node,
	'custom-modal',
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('delConfirmModal'),
					$elm$html$Html$Attributes$class('modal')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('modal-content')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h4,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Sicher?')
								])),
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Alle abgehakten löschen?')
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('modal-footer')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$href('#!'),
									$elm$html$Html$Attributes$class('modal-close waves-effect waves-green btn-flat')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Abbrechen')
								])),
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$href('#!'),
									$elm$html$Html$Attributes$class('modal-close waves-effect waves-green btn-flat'),
									$elm$html$Html$Events$onClick($author$project$Main$DeleteAllDone)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Löschen')
								]))
						]))
				]))
		]));
var $author$project$Main$FilterClicked = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$filterTagChip = function (filterTag) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				'chip green-text green lighten-5' + function () {
					var _v0 = filterTag.aQ;
					if (_v0) {
						return ' darken-1 white-text';
					} else {
						return '';
					}
				}()),
				$elm$html$Html$Events$onClick(
				$author$project$Main$FilterClicked(filterTag.Z))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(filterTag.Z)
			]));
};
var $elm$html$Html$header = _VirtualDom_node('header');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $author$project$Main$NoTagsFilterClicked = {$: 26};
var $author$project$Main$noTagsFilterView = function (isActive) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				'chip green-text green lighten-5' + function () {
					if (isActive) {
						return ' darken-1 white-text';
					} else {
						return '';
					}
				}()),
				$elm$html$Html$Events$onClick($author$project$Main$NoTagsFilterClicked)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('No tags')
			]));
};
var $author$project$FilterTag$sort = function (filterTags) {
	return A2(
		$elm$core$List$sortBy,
		function ($) {
			return $.Z;
		},
		filterTags);
};
var $author$project$Main$SortButtonClicked = {$: 9};
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 3, a: a};
};
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event = F6(
	function (keys, button, clientPos, offsetPos, pagePos, screenPos) {
		return {dS: button, dX: clientPos, ej: keys, ey: offsetPos, eD: pagePos, eP: screenPos};
	});
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton = 4;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton = 0;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton = 5;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton = 1;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton = 2;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton = 3;
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId = function (id) {
	switch (id) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		default:
			return 0;
	}
};
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder = A2(
	$elm$json$Json$Decode$map,
	$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId,
	A2($elm$json$Json$Decode$field, 'button', $elm$json$Json$Decode$int));
var $mpizenberg$elm_pointer_events$Internal$Decode$clientPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$Keys = F4(
	function (alt, ctrl, meta, shift) {
		return {dM: alt, dZ: ctrl, em: meta, eT: shift};
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $mpizenberg$elm_pointer_events$Internal$Decode$keys = A5(
	$elm$json$Json$Decode$map4,
	$mpizenberg$elm_pointer_events$Internal$Decode$Keys,
	A2($elm$json$Json$Decode$field, 'altKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'ctrlKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'metaKey', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool));
var $elm$json$Json$Decode$map6 = _Json_map6;
var $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$pagePos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Internal$Decode$screenPos = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2($elm$json$Json$Decode$field, 'screenX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'screenY', $elm$json$Json$Decode$float));
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder = A7($elm$json$Json$Decode$map6, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event, $mpizenberg$elm_pointer_events$Internal$Decode$keys, $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder, $mpizenberg$elm_pointer_events$Internal$Decode$clientPos, $mpizenberg$elm_pointer_events$Internal$Decode$offsetPos, $mpizenberg$elm_pointer_events$Internal$Decode$pagePos, $mpizenberg$elm_pointer_events$Internal$Decode$screenPos);
var $mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions = F3(
	function (event, options, tag) {
		return A2(
			$elm$html$Html$Events$custom,
			event,
			A2(
				$elm$json$Json$Decode$map,
				function (ev) {
					return {
						u: tag(ev),
						br: options.br,
						by: options.by
					};
				},
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder));
	});
var $author$project$Main$sortButton = function (isActive) {
	return A2(
		$elm$html$Html$a,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$href(''),
				A3(
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions,
				'click',
				{br: true, by: true},
				function (event) {
					return $author$project$Main$SortButtonClicked;
				}),
				$fapian$elm_html_aria$Html$Attributes$Aria$role('button'),
				$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Sortieren')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$i,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						'material-icons' + (isActive ? ' yellow-text' : ' white-text'))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('sort')
					]))
			]));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$headerView = function (model) {
	return A2(
		$elm$html$Html$header,
		_List_fromArray(
			[
				$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Filterbereich')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$nav,
				_List_fromArray(
					[
						$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Header'),
						A2($elm$html$Html$Attributes$style, 'height', 'auto')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('nav-wrapper'),
								A2($elm$html$Html$Attributes$style, 'display', 'flex')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('chips-wrapper filter-chips'),
										$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Filterbereich'),
										$fapian$elm_html_aria$Html$Attributes$Aria$role('navigation')
									]),
								_Utils_ap(
									_List_fromArray(
										[
											$author$project$Main$noTagsFilterView(model.af)
										]),
									A2(
										$elm$core$List$map,
										$author$project$Main$filterTagChip,
										$author$project$FilterTag$sort(model.k)))),
								A2(
								$elm$html$Html$ul,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$id('nav-mobile')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												$author$project$Main$sortButton(model.K)
											])),
										A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$a,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$href('#delConfirmModal'),
														$elm$html$Html$Attributes$class('modal-trigger'),
														$fapian$elm_html_aria$Html$Attributes$Aria$role('button'),
														$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Abgehakte löschen')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$i,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('material-icons white-text')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('delete')
															]))
													]))
											]))
									]))
							]))
					])),
				$author$project$Main$delAllModal
			]));
};
var $author$project$Main$DraftTitleChanged = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Main$FinishEditing = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$CancelEditing = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$cancelButton = function (itemId) {
	return A2(
		$elm$html$Html$a,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$href(''),
				A3(
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions,
				'click',
				{br: true, by: true},
				function (event) {
					return $author$project$Main$CancelEditing(itemId);
				}),
				$elm$html$Html$Attributes$class('grey-text cancel-edit'),
				$fapian$elm_html_aria$Html$Attributes$Aria$role('button')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('Abbrechen')
			]));
};
var $author$project$Main$DeleteItem = function (a) {
	return {$: 21, a: a};
};
var $author$project$Main$deleteCardButton = function (itemId) {
	return A2(
		$elm$html$Html$a,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$href(''),
				A3(
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions,
				'click',
				{br: true, by: true},
				function (event) {
					return $author$project$Main$DeleteItem(itemId);
				}),
				$elm$html$Html$Attributes$class('delete-btn right'),
				$fapian$elm_html_aria$Html$Attributes$Aria$role('button'),
				$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Löschen')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$i,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('material-icons grey-text right-align')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('delete')
					]))
			]));
};
var $author$project$Main$DraftTagsChanged = F2(
	function (a, b) {
		return {$: 19, a: a, b: b};
	});
var $author$project$Main$DraftTagsInputChanged = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$tagElement = function (tagName) {
	return A3(
		$elm$html$Html$node,
		'chips-tag',
		_List_fromArray(
			[
				$elm$html$Html$Attributes$value(tagName)
			]),
		_List_Nil);
};
var $author$project$Main$editChipsView = F2(
	function (filterTags, item) {
		return A3(
			$elm$html$Html$node,
			'custom-chips',
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('chips chips-autocomplete chips-placeholder'),
					$elm$html$Html$Attributes$placeholder('Tags'),
					A2(
					$elm$html$Html$Events$on,
					'tagsChanged',
					A2(
						$elm$json$Json$Decode$map,
						$author$project$Main$DraftTagsChanged(item.aA),
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['detail', 'tags']),
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string)))),
					A2(
					$elm$html$Html$Events$on,
					'inputChanged',
					A2(
						$elm$json$Json$Decode$map,
						$author$project$Main$DraftTagsInputChanged(item.aA),
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['detail', 'remainingText']),
							$elm$json$Json$Decode$string))),
					A2(
					$elm$html$Html$Attributes$attribute,
					'autocompleteOptions',
					A2(
						$elm$json$Json$Encode$encode,
						0,
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, filterTags)))
				]),
			A2($elm$core$List$map, $author$project$Main$tagElement, item.aw));
	});
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$editCard = F2(
	function (filterTags, item) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card s12 item-edit')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card-content')
						]),
					_Utils_ap(
						item.b6 ? _List_Nil : _List_fromArray(
							[
								$author$project$Main$deleteCardButton(item.aA)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('input-field card-title')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$placeholder('Neuer Eintrag'),
												$elm$html$Html$Attributes$type_('text'),
												$elm$html$Html$Attributes$value(item.ay),
												$elm$html$Html$Events$onInput(
												$author$project$Main$DraftTitleChanged(item.aA))
											]),
										_List_Nil)
									])),
								A2($author$project$Main$editChipsView, filterTags, item),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('card-action valign-wrapper justify-right')
									]),
								_List_fromArray(
									[
										$author$project$Main$cancelButton(item.aA),
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('green btn finish-edit'),
												$fapian$elm_html_aria$Html$Attributes$Aria$role('button'),
												$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Bestätigen'),
												$elm$html$Html$Events$onClick(
												$author$project$Main$FinishEditing(item.aA))
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$i,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('material-icons')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('check')
													]))
											]))
									]))
							])))
				]));
	});
var $author$project$Main$CardClicked = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$desyncIcon = A2(
	$elm$html$Html$i,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('material-icons grey-text right-align desync-icon')
		]),
	_List_fromArray(
		[
			$elm$html$Html$text('cloud_off')
		]));
var $author$project$Main$displayTagChip = function (tag) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('chip green-text green lighten-5')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(tag)
			]));
};
var $author$project$Main$EditCardClicked = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$editButton = function (item) {
	return A2(
		$elm$html$Html$a,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$href(''),
				A3(
				$mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions,
				'click',
				{br: true, by: true},
				function (event) {
					return $author$project$Main$EditCardClicked(item.aA);
				}),
				$elm$html$Html$Attributes$class('edit-btn right'),
				$fapian$elm_html_aria$Html$Attributes$Aria$role('button'),
				$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Bearbeiten')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$i,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('material-icons grey-text right-align')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('edit')
					]))
			]));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Main$itemCard = function (itemData) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				'card s12 item-card' + ((itemData.ad === 1) ? ' grey lighten-2 grey-text' : '')),
				$elm$html$Html$Events$onClick(
				$author$project$Main$CardClicked(itemData.aA))
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('card-content')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('right')
							]),
						_List_fromArray(
							[
								itemData.bA ? $elm$html$Html$text('') : $author$project$Main$desyncIcon,
								$author$project$Main$editButton(itemData)
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class(
								'card-title prevent-select' + ((itemData.ad === 1) ? ' line-through' : ''))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(itemData.ag)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('chips-wrapper')
							]),
						A2($elm$core$List$map, $author$project$Main$displayTagChip, itemData._))
					]))
			]));
};
var $author$project$Main$cardView = F2(
	function (filterTags, itemData) {
		return itemData.aM ? A2($author$project$Main$editCard, filterTags, itemData) : $author$project$Main$itemCard(itemData);
	});
var $author$project$FilterTag$maybeActiveTag = function (filterTag) {
	return filterTag.aQ ? $elm$core$Maybe$Just(filterTag.Z) : $elm$core$Maybe$Nothing;
};
var $author$project$FilterTag$activeTags = function (filterTags) {
	return A2($elm$core$List$filterMap, $author$project$FilterTag$maybeActiveTag, filterTags);
};
var $author$project$Main$isVisible = F2(
	function (model, item) {
		var filters = $author$project$FilterTag$activeTags(model.k);
		var filteringActive = ($elm$core$List$length(filters) > 0) || model.af;
		return ((!filteringActive) || (item.aM || (model.af && (!$elm$core$List$length(item._))))) ? true : A3(
			$elm$core$List$foldl,
			$elm$core$Basics$or,
			false,
			A2(
				$elm$core$List$map,
				function (x) {
					return A2($elm$core$List$member, x, filters);
				},
				item._));
	});
var $author$project$Main$sortItems = F2(
	function (useOverrideIndex, items) {
		return useOverrideIndex ? $elm$core$List$reverse(
			A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.ch;
				},
				items)) : $elm$core$List$reverse(
			A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.bp;
				},
				items));
	});
var $author$project$Main$itemsToShow = function (model) {
	return A2(
		$author$project$Main$sortItems,
		model.K,
		A2(
			$elm$core$List$filter,
			$author$project$Main$isVisible(model),
			$elm$core$Dict$values(model.b)));
};
var $author$project$Main$itemCardsView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('row')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('col s12 l8 offset-l2')
					]),
				A2(
					$elm$core$List$map,
					$author$project$Main$cardView(
						A2(
							$elm$core$List$map,
							function ($) {
								return $.Z;
							},
							model.k)),
					$author$project$Main$itemsToShow(model)))
			]));
};
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Main$headerView(model),
				A2(
				$elm$html$Html$main_,
				_List_fromArray(
					[
						$fapian$elm_html_aria$Html$Attributes$Aria$ariaLabel('Listenbereich')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('container')
							]),
						_List_fromArray(
							[
								$author$project$Main$itemCardsView(model),
								$author$project$Main$addCardButton
							]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{eg: $author$project$Main$init, eW: $author$project$Main$subscriptions, e3: $author$project$Main$update, e5: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));