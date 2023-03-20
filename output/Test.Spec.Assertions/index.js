// Generated by purs version 0.15.7
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
var AnyShow = function (x) {
    return x;
};
var showAnyShow = {
    show: $foreign.unsafeStringify
};
var newtypeAnyShow = {
    Coercible0: function () {
        return undefined;
    }
};
var eqAnyShow = function (dictEq) {
    return dictEq;
};
var fail = function (dictMonadThrow) {
    var $98 = Control_Monad_Error_Class.throwError(dictMonadThrow);
    return function ($99) {
        return $98(Effect_Exception.error($99));
    };
};
var shouldContain = function (dictMonadThrow) {
    var when = Control_Applicative.when((dictMonadThrow.Monad0()).Applicative0());
    var fail1 = fail(dictMonadThrow);
    return function (dictShow) {
        var show = Data_Show.show(dictShow);
        return function (dictEq) {
            return function (dictShow1) {
                var show1 = Data_Show.show(dictShow1);
                return function (dictFoldable) {
                    var notElem = Data_Foldable.notElem(dictFoldable)(dictEq);
                    return function (c) {
                        return function (e) {
                            return when(notElem(e)(c))(fail1(show(e) + (" \u2209 " + show1(c))));
                        };
                    };
                };
            };
        };
    };
};
var shouldEqual = function (dictMonadThrow) {
    var when = Control_Applicative.when((dictMonadThrow.Monad0()).Applicative0());
    var fail1 = fail(dictMonadThrow);
    return function (dictShow) {
        var show = Data_Show.show(dictShow);
        return function (dictEq) {
            var notEq = Data_Eq.notEq(dictEq);
            return function (v1) {
                return function (v2) {
                    return when(notEq(v1)(v2))(fail1(show(v1) + (" \u2260 " + show(v2))));
                };
            };
        };
    };
};
var shouldReturn = function (dictMonadThrow) {
    var bind = Control_Bind.bind((dictMonadThrow.Monad0()).Bind1());
    var shouldEqual1 = shouldEqual(dictMonadThrow);
    return function (dictEq) {
        return function (dictShow) {
            var shouldEqual2 = shouldEqual1(dictShow)(dictEq);
            return function (ft) {
                return function (t) {
                    return bind(ft)(function (v) {
                        return shouldEqual2(v)(t);
                    });
                };
            };
        };
    };
};
var shouldNotContain = function (dictMonadThrow) {
    var when = Control_Applicative.when((dictMonadThrow.Monad0()).Applicative0());
    var fail1 = fail(dictMonadThrow);
    return function (dictShow) {
        var show = Data_Show.show(dictShow);
        return function (dictEq) {
            return function (dictShow1) {
                var show1 = Data_Show.show(dictShow1);
                return function (dictFoldable) {
                    var elem = Data_Foldable.elem(dictFoldable)(dictEq);
                    return function (c) {
                        return function (e) {
                            return when(elem(e)(c))(fail1(show(e) + (" \u2208 " + show1(c))));
                        };
                    };
                };
            };
        };
    };
};
var shouldNotEqual = function (dictMonadThrow) {
    var when = Control_Applicative.when((dictMonadThrow.Monad0()).Applicative0());
    var fail1 = fail(dictMonadThrow);
    return function (dictShow) {
        var show = Data_Show.show(dictShow);
        return function (dictEq) {
            var eq = Data_Eq.eq(dictEq);
            return function (v1) {
                return function (v2) {
                    return when(eq(v1)(v2))(fail1(show(v1) + (" = " + show(v2))));
                };
            };
        };
    };
};
var shouldNotReturn = function (dictMonadThrow) {
    var bind = Control_Bind.bind((dictMonadThrow.Monad0()).Bind1());
    var shouldNotEqual1 = shouldNotEqual(dictMonadThrow);
    return function (dictEq) {
        return function (dictShow) {
            var shouldNotEqual2 = shouldNotEqual1(dictShow)(dictEq);
            return function (ft) {
                return function (t) {
                    return bind(ft)(function (v) {
                        return shouldNotEqual2(v)(t);
                    });
                };
            };
        };
    };
};
var shouldNotSatisfy = function (dictMonadThrow) {
    var when = Control_Applicative.when((dictMonadThrow.Monad0()).Applicative0());
    var fail1 = fail(dictMonadThrow);
    return function (dictShow) {
        var show = Data_Show.show(dictShow);
        return function (v) {
            return function (pred) {
                return when(pred(v))(fail1(show(v) + " satisfies predicate, but should not"));
            };
        };
    };
};
var shouldSatisfy = function (dictMonadThrow) {
    var unless = Control_Applicative.unless((dictMonadThrow.Monad0()).Applicative0());
    var fail1 = fail(dictMonadThrow);
    return function (dictShow) {
        var show = Data_Show.show(dictShow);
        return function (v) {
            return function (pred) {
                return unless(pred(v))(fail1(show(v) + " doesn't satisfy predicate"));
            };
        };
    };
};
var expectError = function (dictMonadError) {
    var MonadThrow0 = dictMonadError.MonadThrow0();
    var Monad0 = MonadThrow0.Monad0();
    var bind = Control_Bind.bind(Monad0.Bind1());
    var $$try = Control_Monad_Error_Class["try"](dictMonadError);
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    var throwError = Control_Monad_Error_Class.throwError(MonadThrow0);
    return function (a) {
        return bind($$try(a))(function (e) {
            if (e instanceof Data_Either.Left) {
                return pure(Data_Unit.unit);
            };
            if (e instanceof Data_Either.Right) {
                return throwError(Effect_Exception.error("Expected error"));
            };
            throw new Error("Failed pattern match at Test.Spec.Assertions (line 122, column 3 - line 124, column 51): " + [ e.constructor.name ]);
        });
    };
};
export {
    AnyShow,
    expectError,
    fail,
    shouldContain,
    shouldEqual,
    shouldNotContain,
    shouldNotEqual,
    shouldNotReturn,
    shouldNotSatisfy,
    shouldReturn,
    shouldSatisfy,
    newtypeAnyShow,
    eqAnyShow,
    showAnyShow
};
