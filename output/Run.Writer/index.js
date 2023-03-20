// Generated by purs version 0.15.7
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor_Variant from "../Data.Functor.Variant/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Run from "../Run/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var lift = /* #__PURE__ */ Run.lift();
var on = /* #__PURE__ */ Data_Functor_Variant.on();
var bind = /* #__PURE__ */ Control_Bind.bind(Run.bindRun);
var pure = /* #__PURE__ */ Control_Applicative.pure(Run.applicativeRun);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Run.bindRun);
var writerIsSymbol = {
    reflectSymbol: function () {
        return "writer";
    }
};
var Writer = /* #__PURE__ */ (function () {
    function Writer(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Writer.create = function (value0) {
        return function (value1) {
            return new Writer(value0, value1);
        };
    };
    return Writer;
})();
var functorWriter = {
    map: function (f) {
        return function (m) {
            return new Writer(m.value0, f(m.value1));
        };
    }
};
var liftWriterAt = function (dictIsSymbol) {
    var lift1 = lift(dictIsSymbol)(functorWriter);
    return function () {
        return lift1;
    };
};
var tellAt = function (dictIsSymbol) {
    var liftWriterAt1 = liftWriterAt(dictIsSymbol)();
    return function () {
        return function (sym) {
            return function (w) {
                return liftWriterAt1(sym)(new Writer(w, Data_Unit.unit));
            };
        };
    };
};
var foldWriterAt = function (dictIsSymbol) {
    var on1 = on(dictIsSymbol);
    return function () {
        return function (sym) {
            var handle = on1(sym)(Data_Either.Left.create)(Data_Either.Right.create);
            var loop = function ($copy_k) {
                return function ($copy_w) {
                    return function ($copy_r) {
                        var $tco_var_k = $copy_k;
                        var $tco_var_w = $copy_w;
                        var $tco_done = false;
                        var $tco_result;
                        function $tco_loop(k, w, r) {
                            var v = Run.peel(r);
                            if (v instanceof Data_Either.Left) {
                                var v1 = handle(v.value0);
                                if (v1 instanceof Data_Either.Left) {
                                    $tco_var_k = k;
                                    $tco_var_w = k(w)(v1.value0.value0);
                                    $copy_r = v1.value0.value1;
                                    return;
                                };
                                if (v1 instanceof Data_Either.Right) {
                                    $tco_done = true;
                                    return bind(Run.send(v1.value0))(foldWriterAt(dictIsSymbol)()(sym)(k)(w));
                                };
                                throw new Error("Failed pattern match at Run.Writer (line 101, column 15 - line 105, column 45): " + [ v1.constructor.name ]);
                            };
                            if (v instanceof Data_Either.Right) {
                                $tco_done = true;
                                return pure(new Data_Tuple.Tuple(w, v.value0));
                            };
                            throw new Error("Failed pattern match at Run.Writer (line 100, column 16 - line 107, column 23): " + [ v.constructor.name ]);
                        };
                        while (!$tco_done) {
                            $tco_result = $tco_loop($tco_var_k, $tco_var_w, $copy_r);
                        };
                        return $tco_result;
                    };
                };
            };
            return loop;
        };
    };
};
var runWriterAt = function (dictIsSymbol) {
    var foldWriterAt1 = foldWriterAt(dictIsSymbol)();
    return function (dictMonoid) {
        var append = Data_Semigroup.append(dictMonoid.Semigroup0());
        var mempty = Data_Monoid.mempty(dictMonoid);
        return function () {
            return function (sym) {
                return foldWriterAt1(sym)(append)(mempty);
            };
        };
    };
};
var runWriterAt1 = /* #__PURE__ */ runWriterAt(writerIsSymbol);
var censorAt = function (dictIsSymbol) {
    var on1 = on(dictIsSymbol);
    var tellAt1 = tellAt(dictIsSymbol)();
    return function () {
        return function (sym) {
            var handle = on1(sym)(Data_Either.Left.create)(Data_Either.Right.create);
            var loop = function (f) {
                return function (r) {
                    var v = Run.peel(r);
                    if (v instanceof Data_Either.Left) {
                        var v1 = handle(v.value0);
                        if (v1 instanceof Data_Either.Left) {
                            return discard(tellAt1(sym)(f(v1.value0.value0)))(function () {
                                return loop(f)(v1.value0.value1);
                            });
                        };
                        if (v1 instanceof Data_Either.Right) {
                            return bind(Run.send(v.value0))(loop(f));
                        };
                        throw new Error("Failed pattern match at Run.Writer (line 76, column 15 - line 81, column 30): " + [ v1.constructor.name ]);
                    };
                    if (v instanceof Data_Either.Right) {
                        return pure(v.value0);
                    };
                    throw new Error("Failed pattern match at Run.Writer (line 75, column 14 - line 83, column 13): " + [ v.constructor.name ]);
                };
            };
            return loop;
        };
    };
};
var _writer = /* #__PURE__ */ (function () {
    return Type_Proxy["Proxy"].value;
})();
var censor = /* #__PURE__ */ censorAt(writerIsSymbol)()(_writer);
var foldWriter = /* #__PURE__ */ foldWriterAt(writerIsSymbol)()(_writer);
var liftWriter = /* #__PURE__ */ liftWriterAt(writerIsSymbol)()(_writer);
var runWriter = function (dictMonoid) {
    return runWriterAt1(dictMonoid)()(_writer);
};
var tell = /* #__PURE__ */ tellAt(writerIsSymbol)()(_writer);
export {
    Writer,
    _writer,
    liftWriter,
    liftWriterAt,
    tell,
    tellAt,
    censor,
    censorAt,
    foldWriter,
    foldWriterAt,
    runWriter,
    runWriterAt,
    functorWriter
};
