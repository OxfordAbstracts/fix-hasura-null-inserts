// Generated by purs version 0.15.7
import * as $foreign from "./foreign.js";
import * as Control_Promise from "../Control.Promise/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Data_Functor.functorArray);
var mapFlipped1 = /* #__PURE__ */ Data_Functor.mapFlipped(Effect_Aff.functorAff);
var keyTables = function (tables) {
    var keyArr = function (dictOrd) {
        var fromFoldable = Data_Map_Internal.fromFoldable(dictOrd)(Data_Foldable.foldableArray);
        return function (fn) {
            return function (arr) {
                return fromFoldable(mapFlipped(arr)(function (v) {
                    return new Data_Tuple.Tuple(fn(v), v);
                }));
            };
        };
    };
    var keyArr1 = keyArr(Data_Ord.ordString);
    return keyArr1(function (v) {
        return v.name;
    })(mapFlipped(tables)(function (t) {
        return {
            columns: keyArr1(function (v1) {
                return v1.name;
            })(t.columns),
            name: t.name
        };
    }));
};
var getTables = function ($10) {
    return Control_Promise.toAffE($foreign.getTablesImpl($10));
};
var getTablesKeyed = function (opts) {
    return mapFlipped1(getTables(opts))(keyTables);
};
export {
    getTables,
    getTablesKeyed,
    keyTables
};
