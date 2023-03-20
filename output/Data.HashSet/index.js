// Generated by purs version 0.15.7
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_FoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_HashMap from "../Data.HashMap/index.js";
import * as Data_Hashable from "../Data.Hashable/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var foldrWithIndex = /* #__PURE__ */ Data_FoldableWithIndex.foldrWithIndex(Data_HashMap.foldableWithIndexHashMap);
var foldlWithIndex = /* #__PURE__ */ Data_FoldableWithIndex.foldlWithIndex(Data_HashMap.foldableWithIndexHashMap);
var foldMapWithIndex = /* #__PURE__ */ Data_FoldableWithIndex.foldMapWithIndex(Data_HashMap.foldableWithIndexHashMap);
var HashSet = function (x) {
    return x;
};
var union = function (dictHashable) {
    var unionWith = Data_HashMap.unionWith(dictHashable);
    return function (v) {
        return function (v1) {
            return unionWith(Data_Function["const"])(v)(v1);
        };
    };
};
var toMap = function (v) {
    return v;
};
var toArray = function (v) {
    return Data_HashMap.keys(v);
};
var toUnfoldable = function (dictUnfoldable) {
    var $103 = Data_Array.toUnfoldable(dictUnfoldable);
    return function ($104) {
        return $103(toArray($104));
    };
};
var size = function (v) {
    return Data_HashMap.size(v);
};
var singleton = function (dictHashable) {
    var singleton1 = Data_HashMap.singleton(dictHashable);
    return function (a) {
        return singleton1(a)(Data_Unit.unit);
    };
};
var showHashSet = function (dictShow) {
    var show = Data_Show.show(Data_Show.showArray(dictShow));
    return {
        show: function (s) {
            return "(fromFoldable " + (show(toArray(s)) + ")");
        }
    };
};
var semigroupHashSet = function (dictHashable) {
    return {
        append: union(dictHashable)
    };
};
var member = function (dictHashable) {
    var member1 = Data_HashMap.member(dictHashable);
    return function (a) {
        return function (v) {
            return member1(a)(v);
        };
    };
};
var isEmpty = function (v) {
    return Data_HashMap.isEmpty(v);
};
var intersection = function (dictHashable) {
    var intersectionWith = Data_HashMap.intersectionWith(dictHashable);
    return function (v) {
        return function (v1) {
            return intersectionWith(Data_Function["const"])(v)(v1);
        };
    };
};
var insert = function (dictHashable) {
    var insert1 = Data_HashMap.insert(dictHashable);
    return function (a) {
        return function (v) {
            return insert1(a)(Data_Unit.unit)(v);
        };
    };
};
var hashableHashSet = function (dictHashable) {
    return Data_HashMap.hashHashMap(dictHashable)(Data_Hashable.hashableUnit);
};
var fromMap = HashSet;
var fromArray = function (dictHashable) {
    var $105 = Data_HashMap.fromArrayBy(dictHashable)(identity)(Data_Function["const"](Data_Unit.unit));
    return function ($106) {
        return HashSet($105($106));
    };
};
var foldableHashSet = {
    foldr: function (f) {
        return function (a) {
            return function (v) {
                return foldrWithIndex(function (k) {
                    return function (v1) {
                        return f(k);
                    };
                })(a)(v);
            };
        };
    },
    foldl: function (f) {
        return function (a) {
            return function (v) {
                return foldlWithIndex(function (k) {
                    return function (b) {
                        return function (v1) {
                            return f(b)(k);
                        };
                    };
                })(a)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        var foldMapWithIndex1 = foldMapWithIndex(dictMonoid);
        return function (f) {
            return function (v) {
                return foldMapWithIndex1(function (k) {
                    return function (v1) {
                        return f(k);
                    };
                })(v);
            };
        };
    }
};
var foldr = /* #__PURE__ */ Data_Foldable.foldr(foldableHashSet);
var filter = function (f) {
    return function (v) {
        return Data_HashMap.filterWithKey(function (k) {
            return function (v1) {
                return f(k);
            };
        })(v);
    };
};
var eqHashSet = function (dictEq) {
    return Data_HashMap.eqHashMap(dictEq)(Data_Eq.eqUnit);
};
var empty = Data_HashMap.empty;
var fromFoldable = function (dictFoldable) {
    var foldr1 = Data_Foldable.foldr(dictFoldable);
    return function (dictHashable) {
        return foldr1(insert(dictHashable))(empty);
    };
};
var map = function (dictHashable) {
    var insert1 = insert(dictHashable);
    return function (f) {
        return foldr(function (x) {
            return insert1(f(x));
        })(empty);
    };
};
var mapMaybe = function (dictHashable) {
    var insert1 = insert(dictHashable);
    return function (f) {
        return foldr(function (a) {
            return function (s) {
                var v = f(a);
                if (v instanceof Data_Maybe.Nothing) {
                    return s;
                };
                if (v instanceof Data_Maybe.Just) {
                    return insert1(v.value0)(s);
                };
                throw new Error("Failed pattern match at Data.HashSet (line 124, column 18 - line 126, column 33): " + [ v.constructor.name ]);
            };
        })(empty);
    };
};
var monoidHashSet = function (dictHashable) {
    var semigroupHashSet1 = semigroupHashSet(dictHashable);
    return {
        mempty: empty,
        Semigroup0: function () {
            return semigroupHashSet1;
        }
    };
};
var unions = function (dictFoldable) {
    var fold = Data_Foldable.fold(dictFoldable);
    return function (dictHashable) {
        return fold(monoidHashSet(dictHashable));
    };
};
var difference = function (dictHashable) {
    var difference1 = Data_HashMap.difference(dictHashable);
    return function (v) {
        return function (v1) {
            return difference1(v)(v1);
        };
    };
};
var $$delete = function (dictHashable) {
    var delete1 = Data_HashMap["delete"](dictHashable);
    return function (a) {
        return function (v) {
            return delete1(a)(v);
        };
    };
};
export {
    empty,
    singleton,
    insert,
    member,
    $$delete as delete,
    map,
    filter,
    mapMaybe,
    union,
    unions,
    intersection,
    difference,
    size,
    isEmpty,
    fromArray,
    fromFoldable,
    fromMap,
    toArray,
    toMap,
    toUnfoldable,
    eqHashSet,
    hashableHashSet,
    semigroupHashSet,
    monoidHashSet,
    showHashSet,
    foldableHashSet
};
