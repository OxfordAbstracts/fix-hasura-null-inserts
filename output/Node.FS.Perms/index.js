// Generated by purs version 0.15.7
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var toEnum = /* #__PURE__ */ Data_Enum.toEnum(Data_Enum.boundedEnumChar);
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Data_Eq.eqChar));
var apply = /* #__PURE__ */ Control_Apply.apply(Data_Maybe.applyMaybe);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var compare = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(Data_Ord.ordBoolean));
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var Perm = function (x) {
    return x;
};
var Perms = function (x) {
    return x;
};
var write = {
    r: false,
    w: true,
    x: false
};
var semiringPerm = {
    add: function (v) {
        return function (v1) {
            return {
                r: v.r || v1.r,
                w: v.w || v1.w,
                x: v.x || v1.x
            };
        };
    },
    zero: {
        r: false,
        w: false,
        x: false
    },
    mul: function (v) {
        return function (v1) {
            return {
                r: v.r && v1.r,
                w: v.w && v1.w,
                x: v.x && v1.x
            };
        };
    },
    one: {
        r: true,
        w: true,
        x: true
    }
};
var add1 = /* #__PURE__ */ Data_Semiring.add(semiringPerm);
var read = {
    r: true,
    w: false,
    x: false
};
var permToInt = function (v) {
    return ((function () {
        if (v.r) {
            return 4;
        };
        return 0;
    })() + (function () {
        if (v.w) {
            return 2;
        };
        return 0;
    })() | 0) + (function () {
        if (v.x) {
            return 1;
        };
        return 0;
    })() | 0;
};
var permToString = /* #__PURE__ */ (function () {
    var $119 = Data_Show.show(Data_Show.showInt);
    return function ($120) {
        return $119(permToInt($120));
    };
})();
var permsToString = function (v) {
    return "0" + (permToString(v.u) + (permToString(v.g) + permToString(v.o)));
};
var permsToInt = /* #__PURE__ */ (function () {
    var $121 = Data_Int.fromStringAs(Data_Int.octal);
    return function ($122) {
        return fromJust($121(permsToString($122)));
    };
})();
var none = /* #__PURE__ */ Data_Semiring.zero(semiringPerm);
var mkPerms = function (u) {
    return function (g) {
        return function (o) {
            return {
                u: u,
                g: g,
                o: o
            };
        };
    };
};
var mkPerm = function (r) {
    return function (w) {
        return function (x) {
            return {
                r: r,
                w: w,
                x: x
            };
        };
    };
};
var execute = {
    r: false,
    w: false,
    x: true
};
var permFromChar = function (c) {
    if (c === "0") {
        return new Data_Maybe.Just(none);
    };
    if (c === "1") {
        return new Data_Maybe.Just(execute);
    };
    if (c === "2") {
        return new Data_Maybe.Just(write);
    };
    if (c === "3") {
        return new Data_Maybe.Just(add1(write)(execute));
    };
    if (c === "4") {
        return new Data_Maybe.Just(read);
    };
    if (c === "5") {
        return new Data_Maybe.Just(add1(read)(execute));
    };
    if (c === "6") {
        return new Data_Maybe.Just(add1(read)(write));
    };
    if (c === "7") {
        return new Data_Maybe.Just(add1(add1(read)(write))(execute));
    };
    return Data_Maybe.Nothing.value;
};
var permsFromString = /* #__PURE__ */ (function () {
    var zeroChar = fromJust(toEnum(48));
    var dropPrefix = function (x) {
        return function (xs) {
            if (eq(Data_String_CodeUnits.charAt(0)(xs))(new Data_Maybe.Just(x))) {
                return Data_String_CodePoints.drop(1)(xs);
            };
            if (Data_Boolean.otherwise) {
                return xs;
            };
            throw new Error("Failed pattern match at Node.FS.Perms (line 126, column 5 - line 128, column 35): " + [ x.constructor.name, xs.constructor.name ]);
        };
    };
    var _perms = function (v) {
        if (v.length === 3) {
            return apply(apply(map(mkPerms)(permFromChar(v[0])))(permFromChar(v[1])))(permFromChar(v[2]));
        };
        return Data_Maybe.Nothing.value;
    };
    var $123 = dropPrefix(zeroChar);
    return function ($124) {
        return _perms(Data_String_CodeUnits.toCharArray($123($124)));
    };
})();
var eqPerm = {
    eq: function (v) {
        return function (v1) {
            return v.r === v1.r && (v.w === v1.w && v.x === v1.x);
        };
    }
};
var eq2 = /* #__PURE__ */ Data_Eq.eq(eqPerm);
var eqPerms = {
    eq: function (v) {
        return function (v1) {
            return eq2(v.u)(v1.u) && (eq2(v.g)(v1.g) && eq2(v.o)(v1.o));
        };
    }
};
var ordPerm = {
    compare: function (v) {
        return function (v1) {
            return compare([ v.r, v.w, v.x ])([ v1.r, v1.w, v1.x ]);
        };
    },
    Eq0: function () {
        return eqPerm;
    }
};
var compare1 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(ordPerm));
var ordPerms = {
    compare: function (v) {
        return function (v1) {
            return compare1([ v.u, v.g, v.o ])([ v1.u, v1.g, v1.o ]);
        };
    },
    Eq0: function () {
        return eqPerms;
    }
};
var all = /* #__PURE__ */ Data_Semiring.one(semiringPerm);
var showPerm = {
    show: function (v) {
        if (eq2(v)(none)) {
            return "none";
        };
        if (eq2(v)(all)) {
            return "all";
        };
        var ps = append1((function () {
            if (v.r) {
                return [ "read" ];
            };
            return [  ];
        })())(append1((function () {
            if (v.w) {
                return [ "write" ];
            };
            return [  ];
        })())((function () {
            if (v.x) {
                return [ "execute" ];
            };
            return [  ];
        })()));
        return Data_String_Common.joinWith(" + ")(ps);
    }
};
var show = /* #__PURE__ */ Data_Show.show(showPerm);
var showPerms = {
    show: function (v) {
        var f = function (perm) {
            var str = show(perm);
            var $115 = Data_Maybe.isNothing(Data_String_CodePoints.indexOf(" ")(str));
            if ($115) {
                return str;
            };
            return "(" + (str + ")");
        };
        return "mkPerms " + Data_String_Common.joinWith(" ")(map1(f)([ v.u, v.g, v.o ]));
    }
};
export {
    mkPerm,
    none,
    read,
    write,
    execute,
    all,
    mkPerms,
    permsFromString,
    permsToString,
    permsToInt,
    eqPerm,
    ordPerm,
    showPerm,
    semiringPerm,
    eqPerms,
    ordPerms,
    showPerms
};
