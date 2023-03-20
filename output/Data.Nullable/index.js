// Generated by purs version 0.15.7
import * as $foreign from "./foreign.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
var toNullable = /* #__PURE__ */ Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return {
        show: (function () {
            var $17 = Data_Maybe.maybe("null")(Data_Show.show(dictShow));
            return function ($18) {
                return $17(toMaybe($18));
            };
        })()
    };
};
var eqNullable = function (dictEq) {
    return {
        eq: Data_Function.on(Data_Eq.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe)
    };
};
var ordNullable = function (dictOrd) {
    var eqNullable1 = eqNullable(dictOrd.Eq0());
    return {
        compare: Data_Function.on(Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe),
        Eq0: function () {
            return eqNullable1;
        }
    };
};
var eq1Nullable = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqNullable(dictEq));
    }
};
var ord1Nullable = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordNullable(dictOrd));
    },
    Eq10: function () {
        return eq1Nullable;
    }
};
export {
    null,
    notNull
} from "./foreign.js";
export {
    toMaybe,
    toNullable,
    showNullable,
    eqNullable,
    eq1Nullable,
    ordNullable,
    ord1Nullable
};
