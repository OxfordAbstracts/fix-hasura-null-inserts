// Generated by purs version 0.15.7
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Time_Duration from "../Data.Time.Duration/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Test_Spec_Speed from "../Test.Spec.Speed/index.js";
var show = /* #__PURE__ */ Data_Show.show(Test_Spec_Speed.showSpeed);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Time_Duration.showMilliseconds);
var eq = /* #__PURE__ */ Data_Eq.eq(Test_Spec_Speed.showEq);
var eq1 = /* #__PURE__ */ Data_Eq.eq(Data_Time_Duration.eqMilliseconds);
var eq2 = /* #__PURE__ */ Data_Eq.eq(Data_Eq.eqString);
var eq3 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Data_Eq.eqString));
var Success = /* #__PURE__ */ (function () {
    function Success(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Success.create = function (value0) {
        return function (value1) {
            return new Success(value0, value1);
        };
    };
    return Success;
})();
var Failure = /* #__PURE__ */ (function () {
    function Failure(value0) {
        this.value0 = value0;
    };
    Failure.create = function (value0) {
        return new Failure(value0);
    };
    return Failure;
})();
var showResult = {
    show: function (v) {
        if (v instanceof Success) {
            return "Success (" + (show(v.value0) + (" " + (show1(v.value1) + ")")));
        };
        if (v instanceof Failure) {
            return "Failure (Error " + (Effect_Exception.message(v.value0) + ")");
        };
        throw new Error("Failed pattern match at Test.Spec.Result (line 15, column 1 - line 17, column 69): " + [ v.constructor.name ]);
    }
};
var eqResult = {
    eq: function (v) {
        return function (v1) {
            if (v instanceof Success && v1 instanceof Success) {
                return eq(v.value0)(v1.value0) && eq1(v.value1)(v1.value1);
            };
            if (v instanceof Failure && v1 instanceof Failure) {
                return Data_Function.on(eq2)(Effect_Exception.message)(v.value0)(v1.value0) && Data_Function.on(eq3)(Effect_Exception.stack)(v.value0)(v1.value0);
            };
            return false;
        };
    }
};
export {
    Success,
    Failure,
    showResult,
    eqResult
};