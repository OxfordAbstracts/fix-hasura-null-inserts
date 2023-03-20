// Generated by purs version 0.15.7
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Lens_Index from "../Data.Lens.Index/index.js";
import * as Data_Lens_Lens from "../Data.Lens.Lens/index.js";
import * as Data_Lens_Setter from "../Data.Lens.Setter/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Profunctor_Strong from "../Data.Profunctor.Strong/index.js";
import * as Data_Set from "../Data.Set/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var atSet = function (dictOrd) {
    var $$delete = Data_Set["delete"](dictOrd);
    var insert = Data_Set.insert(dictOrd);
    var member = Data_Set.member(dictOrd);
    var indexSet = Data_Lens_Index.indexSet(dictOrd);
    return {
        at: function (x) {
            return function (dictStrong) {
                var update = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return $$delete(x);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return insert(x);
                    };
                    throw new Error("Failed pattern match at Data.Lens.At (line 50, column 5 - line 50, column 32): " + [ v.constructor.name ]);
                };
                var get = function (xs) {
                    var $28 = member(x)(xs);
                    if ($28) {
                        return new Data_Maybe.Just(Data_Unit.unit);
                    };
                    return Data_Maybe.Nothing.value;
                };
                return Data_Lens_Lens.lens(get)(Data_Function.flip(update))(dictStrong);
            };
        },
        Index0: function () {
            return indexSet;
        }
    };
};
var atMaybe = {
    at: function (v) {
        return function (dictStrong) {
            return Data_Lens_Lens.lens(identity)(function (v1) {
                return identity;
            })(dictStrong);
        };
    },
    Index0: function () {
        return Data_Lens_Index.indexMaybe;
    }
};
var atMap = function (dictOrd) {
    var lookup = Data_Map_Internal.lookup(dictOrd);
    var $$delete = Data_Map_Internal["delete"](dictOrd);
    var insert = Data_Map_Internal.insert(dictOrd);
    var indexMap = Data_Lens_Index.indexMap(dictOrd);
    return {
        at: function (k) {
            return function (dictStrong) {
                return Data_Lens_Lens.lens(lookup(k))(function (m) {
                    return Data_Maybe["maybe$prime"](function (v) {
                        return $$delete(k)(m);
                    })(function (v) {
                        return insert(k)(v)(m);
                    });
                })(dictStrong);
            };
        },
        Index0: function () {
            return indexMap;
        }
    };
};
var atIdentity = {
    at: function (v) {
        return function (dictStrong) {
            return Data_Lens_Lens.lens(function ($30) {
                return Data_Maybe.Just.create(unwrap($30));
            })(Data_Function.flip(Data_Maybe.maybe)(Data_Identity.Identity))(dictStrong);
        };
    },
    Index0: function () {
        return Data_Lens_Index.indexIdentity;
    }
};
var atForeignObject = {
    at: function (k) {
        return function (dictStrong) {
            return Data_Lens_Lens.lens(Foreign_Object.lookup(k))(function (m) {
                return Data_Maybe["maybe$prime"](function (v) {
                    return Foreign_Object["delete"](k)(m);
                })(function (v) {
                    return Foreign_Object.insert(k)(v)(m);
                });
            })(dictStrong);
        };
    },
    Index0: function () {
        return Data_Lens_Index.indexForeignObject;
    }
};
var at = function (dict) {
    return dict.at;
};
var sans = function (dictAt) {
    var at1 = at(dictAt);
    return function (k) {
        return Data_Lens_Setter.set(at1(k)(Data_Profunctor_Strong.strongFn))(Data_Maybe.Nothing.value);
    };
};
export {
    at,
    sans,
    atIdentity,
    atMaybe,
    atSet,
    atMap,
    atForeignObject
};
