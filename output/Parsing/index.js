// Generated by purs version 0.15.7
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordInt);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var compare1 = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordString);
var Position = function (x) {
    return x;
};
var ParseState = /* #__PURE__ */ (function () {
    function ParseState(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    ParseState.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new ParseState(value0, value1, value2);
            };
        };
    };
    return ParseState;
})();
var ParseError = /* #__PURE__ */ (function () {
    function ParseError(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ParseError.create = function (value0) {
        return function (value1) {
            return new ParseError(value0, value1);
        };
    };
    return ParseError;
})();
var ParserT = function (x) {
    return x;
};
var More = /* #__PURE__ */ (function () {
    function More(value0) {
        this.value0 = value0;
    };
    More.create = function (value0) {
        return new More(value0);
    };
    return More;
})();
var Lift = /* #__PURE__ */ (function () {
    function Lift(value0) {
        this.value0 = value0;
    };
    Lift.create = function (value0) {
        return new Lift(value0);
    };
    return Lift;
})();
var Stop = /* #__PURE__ */ (function () {
    function Stop(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Stop.create = function (value0) {
        return function (value1) {
            return new Stop(value0, value1);
        };
    };
    return Stop;
})();
var monadTransParserT = {
    lift: function (dictMonad) {
        var map = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0());
        return function (m) {
            return function (state1, v, lift$prime, v1, done) {
                return lift$prime(map(function (a) {
                    return function (v2) {
                        return done(state1, a);
                    };
                })(m));
            };
        };
    }
};
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(monadTransParserT);
var lazyParserT = {
    defer: function (f) {
        var m = Data_Lazy.defer(f);
        return function (state1, more, lift1, $$throw, done) {
            var v = Data_Lazy.force(m);
            return v(state1, more, lift1, $$throw, done);
        };
    }
};
var genericPosition_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var genericShow = /* #__PURE__ */ Data_Show_Generic.genericShow(genericPosition_)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Show.showRecord()()(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
    reflectSymbol: function () {
        return "column";
    }
})(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
    reflectSymbol: function () {
        return "index";
    }
})(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil({
    reflectSymbol: function () {
        return "line";
    }
})(Data_Show.showInt))(Data_Show.showInt))(Data_Show.showInt))))({
    reflectSymbol: function () {
        return "Position";
    }
}));
var showPosition = {
    show: function (x) {
        return genericShow(x);
    }
};
var show1 = /* #__PURE__ */ Data_Show.show(showPosition);
var functorParserT = {
    map: function (f) {
        return function (v) {
            return function (state1, more, lift1, $$throw, done) {
                return more(function (v1) {
                    return v(state1, more, lift1, $$throw, function (state2, a) {
                        return more(function (v2) {
                            return done(state2, f(a));
                        });
                    });
                });
            };
        };
    }
};
var eqPosition = {
    eq: function (v) {
        return function (v1) {
            return v.index === v1.index;
        };
    }
};
var eq2 = /* #__PURE__ */ Data_Eq.eq(eqPosition);
var ordPosition = {
    compare: function (v) {
        return function (v1) {
            return compare(v.index)(v1.index);
        };
    },
    Eq0: function () {
        return eqPosition;
    }
};
var compare2 = /* #__PURE__ */ Data_Ord.compare(ordPosition);
var applyParserT = {
    apply: function (v) {
        return function (v1) {
            return function (state1, more, lift1, $$throw, done) {
                return more(function (v2) {
                    return v(state1, more, lift1, $$throw, function (state2, f) {
                        return more(function (v3) {
                            return v1(state2, more, lift1, $$throw, function (state3, a) {
                                return more(function (v4) {
                                    return done(state3, f(a));
                                });
                            });
                        });
                    });
                });
            };
        };
    },
    Functor0: function () {
        return functorParserT;
    }
};
var lift2 = /* #__PURE__ */ Control_Apply.lift2(applyParserT);
var bindParserT = {
    bind: function (v) {
        return function (next) {
            return function (state1, more, lift1, $$throw, done) {
                return more(function (v1) {
                    return v(state1, more, lift1, $$throw, function (state2, a) {
                        return more(function (v2) {
                            var v3 = next(a);
                            return v3(state2, more, lift1, $$throw, done);
                        });
                    });
                });
            };
        };
    },
    Apply0: function () {
        return applyParserT;
    }
};
var bindFlipped = /* #__PURE__ */ Control_Bind.bindFlipped(bindParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(bindParserT);
var semigroupParserT = function (dictSemigroup) {
    return {
        append: lift2(Data_Semigroup.append(dictSemigroup))
    };
};
var applicativeParserT = {
    pure: function (a) {
        return function (state1, v, v1, v2, done) {
            return done(state1, a);
        };
    },
    Apply0: function () {
        return applyParserT;
    }
};
var pure = /* #__PURE__ */ Control_Applicative.pure(applicativeParserT);
var monadParserT = {
    Applicative0: function () {
        return applicativeParserT;
    },
    Bind1: function () {
        return bindParserT;
    }
};
var monadAskParserT = function (dictMonadAsk) {
    return {
        ask: lift(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)),
        Monad0: function () {
            return monadParserT;
        }
    };
};
var monadReaderParserT = function (dictMonadReader) {
    var local = Control_Monad_Reader_Class.local(dictMonadReader);
    var monadAskParserT1 = monadAskParserT(dictMonadReader.MonadAsk0());
    return {
        local: function (f) {
            return function (v) {
                return function (state1, more, lift1, $$throw, done) {
                    return v(state1, more, (function () {
                        var $279 = local(f);
                        return function ($280) {
                            return lift1($279($280));
                        };
                    })(), $$throw, done);
                };
            };
        },
        MonadAsk0: function () {
            return monadAskParserT1;
        }
    };
};
var monadRecParserT = {
    tailRecM: function (next) {
        return function (initArg) {
            return function (state1, more, lift1, $$throw, done) {
                var $lazy_loop = $runtime_lazy("loop", "Parsing", function () {
                    return function (state2, arg, gas) {
                        var v = next(arg);
                        return v(state2, more, lift1, $$throw, function (state3, step) {
                            if (step instanceof Control_Monad_Rec_Class.Loop) {
                                var $206 = gas === 0;
                                if ($206) {
                                    return more(function (v1) {
                                        return $lazy_loop(277)(state3, step.value0, 30);
                                    });
                                };
                                return $lazy_loop(279)(state3, step.value0, gas - 1 | 0);
                            };
                            if (step instanceof Control_Monad_Rec_Class.Done) {
                                return done(state3, step.value0);
                            };
                            throw new Error("Failed pattern match at Parsing (line 273, column 39 - line 281, column 43): " + [ step.constructor.name ]);
                        });
                    };
                });
                var loop = $lazy_loop(270);
                return loop(state1, initArg, 30);
            };
        };
    },
    Monad0: function () {
        return monadParserT;
    }
};
var monadStateParserT = function (dictMonadState) {
    var lift1 = lift(dictMonadState.Monad0());
    var state = Control_Monad_State_Class.state(dictMonadState);
    return {
        state: function (k) {
            return lift1(state(k));
        },
        Monad0: function () {
            return monadParserT;
        }
    };
};
var monadThrowParseErrorParse = {
    throwError: function (err) {
        return function (state1, v, v1, $$throw, v2) {
            return $$throw(state1, err);
        };
    },
    Monad0: function () {
        return monadParserT;
    }
};
var throwError = /* #__PURE__ */ Control_Monad_Error_Class.throwError(monadThrowParseErrorParse);
var monadErrorParseErrorParse = {
    catchError: function (v) {
        return function (next) {
            return function (state1, more, lift1, $$throw, done) {
                return more(function (v1) {
                    return v(state1, more, lift1, function (state2, err) {
                        var v2 = next(err);
                        return v2(state2, more, lift1, $$throw, done);
                    }, done);
                });
            };
        };
    },
    MonadThrow0: function () {
        return monadThrowParseErrorParse;
    }
};
var catchError = /* #__PURE__ */ Control_Monad_Error_Class.catchError(monadErrorParseErrorParse);
var monoidParserT = function (dictMonoid) {
    var semigroupParserT1 = semigroupParserT(dictMonoid.Semigroup0());
    return {
        mempty: pure(Data_Monoid.mempty(dictMonoid)),
        Semigroup0: function () {
            return semigroupParserT1;
        }
    };
};
var altParserT = {
    alt: function (v) {
        return function (v1) {
            return function (v2, more, lift1, $$throw, done) {
                return more(function (v3) {
                    return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function (v4, err) {
                        return more(function (v5) {
                            if (v4.value2) {
                                return $$throw(v4, err);
                            };
                            return v1(v2, more, lift1, $$throw, done);
                        });
                    }, done);
                });
            };
        };
    },
    Functor0: function () {
        return functorParserT;
    }
};
var stateParserT = function (k) {
    return function (state1, v, v1, v2, done) {
        var v3 = k(state1);
        return done(v3.value1, v3.value0);
    };
};
var showParseError = {
    show: function (v) {
        return "(ParseError " + (show(v.value0) + (" " + (show1(v.value1) + ")")));
    }
};
var runParserT$prime = function (dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map = Data_Functor.map(((Monad0.Bind1()).Apply0()).Functor0());
    var pure1 = Control_Applicative.pure(Monad0.Applicative0());
    var tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);
    return function (state1) {
        return function (v) {
            var go = function ($copy_step) {
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(step) {
                    var v1 = step(Data_Unit.unit);
                    if (v1 instanceof More) {
                        $copy_step = v1.value0;
                        return;
                    };
                    if (v1 instanceof Lift) {
                        $tco_done = true;
                        return map(Control_Monad_Rec_Class.Loop.create)(v1.value0);
                    };
                    if (v1 instanceof Stop) {
                        $tco_done = true;
                        return pure1(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v1.value1, v1.value0)));
                    };
                    throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [ v1.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($copy_step);
                };
                return $tco_result;
            };
            return tailRecM(go)(function (v1) {
                return v(state1, More.create, Lift.create, function (state2, err) {
                    return new Stop(state2, new Data_Either.Left(err));
                }, function (state2, res) {
                    return new Stop(state2, new Data_Either.Right(res));
                });
            });
        };
    };
};
var region = function (context) {
    return function (p) {
        return catchError(p)(function (err) {
            return throwError(context(err));
        });
    };
};
var position = /* #__PURE__ */ stateParserT(function (v) {
    return new Data_Tuple.Tuple(v.value1, v);
});
var parseErrorPosition = function (v) {
    return v.value1;
};
var parseErrorMessage = function (v) {
    return v.value0;
};
var mapParserT = function (dictMonadRec) {
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function (dictFunctor) {
        var map = Data_Functor.map(dictFunctor);
        return function (f) {
            return function (p) {
                return function (state1, v, lift1, $$throw, done) {
                    return lift1(map(function (v1) {
                        return function (v2) {
                            if (v1.value0 instanceof Data_Either.Left) {
                                return $$throw(v1.value1, v1.value0.value0);
                            };
                            if (v1.value0 instanceof Data_Either.Right) {
                                return done(v1.value1, v1.value0.value0);
                            };
                            throw new Error("Failed pattern match at Parsing (line 192, column 13 - line 196, column 37): " + [ v1.value0.constructor.name ]);
                        };
                    })(f(runParserT$prime1(state1)(p))));
                };
            };
        };
    };
};
var initialPos = {
    index: 0,
    line: 1,
    column: 1
};
var runParserT = function (dictMonadRec) {
    var map = Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function (s) {
        return function (p) {
            var initialState = new ParseState(s, initialPos, false);
            return map(Data_Tuple.fst)(runParserT$prime1(initialState)(p));
        };
    };
};
var runParserT1 = /* #__PURE__ */ runParserT(Control_Monad_Rec_Class.monadRecIdentity);
var runParser = function (s) {
    var $281 = runParserT1(s);
    return function ($282) {
        return unwrap($281($282));
    };
};
var hoistParserT = function (f) {
    return function (v) {
        return function (state1, more, lift1, $$throw, done) {
            return v(state1, more, function ($283) {
                return lift1(f($283));
            }, $$throw, done);
        };
    };
};
var getParserT = function (state1, v, v1, v2, done) {
    return done(state1, state1);
};
var failWithPosition = function (message) {
    return function (pos) {
        return throwError(new ParseError(message, pos));
    };
};
var fail = function (message) {
    return bindFlipped(failWithPosition(message))(position);
};
var liftEither = function (dictMonad) {
    return function (f) {
        if (f instanceof Data_Either.Left) {
            return fail(f.value0);
        };
        if (f instanceof Data_Either.Right) {
            return pure(f.value0);
        };
        throw new Error("Failed pattern match at Parsing (line 515, column 16 - line 517, column 20): " + [ f.constructor.name ]);
    };
};
var liftExceptT = function (dictMonad) {
    var lift1 = lift(dictMonad);
    return function (f) {
        return bind(lift1(Control_Monad_Except_Trans.runExceptT(f)))(function (v) {
            if (v instanceof Data_Either.Left) {
                return fail(v.value0);
            };
            if (v instanceof Data_Either.Right) {
                return pure(v.value0);
            };
            throw new Error("Failed pattern match at Parsing (line 529, column 41 - line 531, column 20): " + [ v.constructor.name ]);
        });
    };
};
var liftMaybe = function (dictMonad) {
    return function (message) {
        return function (f) {
            if (f instanceof Data_Maybe.Nothing) {
                return fail(message(Data_Unit.unit));
            };
            if (f instanceof Data_Maybe.Just) {
                return pure(f.value0);
            };
            throw new Error("Failed pattern match at Parsing (line 501, column 23 - line 503, column 19): " + [ f.constructor.name ]);
        };
    };
};
var plusParserT = {
    empty: /* #__PURE__ */ fail("No alternative"),
    Alt0: function () {
        return altParserT;
    }
};
var alternativeParserT = {
    Applicative0: function () {
        return applicativeParserT;
    },
    Plus1: function () {
        return plusParserT;
    }
};
var monadPlusParserT = {
    Monad0: function () {
        return monadParserT;
    },
    Alternative1: function () {
        return alternativeParserT;
    }
};
var eqParseError = {
    eq: function (x) {
        return function (y) {
            return x.value0 === y.value0 && eq2(x.value1)(y.value1);
        };
    }
};
var ordParseError = {
    compare: function (x) {
        return function (y) {
            var v = compare1(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return compare2(x.value1)(y.value1);
        };
    },
    Eq0: function () {
        return eqParseError;
    }
};
var consume = /* #__PURE__ */ stateParserT(function (v) {
    return new Data_Tuple.Tuple(Data_Unit.unit, new ParseState(v.value0, v.value1, true));
});
export {
    runParser,
    ParserT,
    runParserT,
    runParserT$prime,
    ParseError,
    parseErrorMessage,
    parseErrorPosition,
    Position,
    initialPos,
    consume,
    position,
    fail,
    failWithPosition,
    region,
    liftMaybe,
    liftEither,
    liftExceptT,
    ParseState,
    stateParserT,
    getParserT,
    hoistParserT,
    mapParserT,
    showParseError,
    eqParseError,
    ordParseError,
    lazyParserT,
    semigroupParserT,
    monoidParserT,
    functorParserT,
    applyParserT,
    applicativeParserT,
    bindParserT,
    monadParserT,
    monadRecParserT,
    monadStateParserT,
    monadAskParserT,
    monadReaderParserT,
    monadThrowParseErrorParse,
    monadErrorParseErrorParse,
    altParserT,
    plusParserT,
    alternativeParserT,
    monadPlusParserT,
    monadTransParserT,
    genericPosition_,
    showPosition,
    eqPosition,
    ordPosition
};