// Generated by purs version 0.15.7
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Dotenv_Internal_Environment from "../Dotenv.Internal.Environment/index.js";
import * as Run from "../Run/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Run.bindRun);
var pure = /* #__PURE__ */ Control_Applicative.pure(Run.applicativeRun);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Run.bindRun);
var when = /* #__PURE__ */ Control_Applicative.when(Run.applicativeRun);
var applySettings = /* #__PURE__ */ Data_Traversable.traverse(Data_Traversable.traversableArray)(Run.applicativeRun)(function (v) {
    return bind(Dotenv_Internal_Environment.lookupEnv(v.value0))(function (currentValue) {
        var $9 = Data_Maybe.isJust(currentValue);
        if ($9) {
            return pure(new Data_Tuple.Tuple(v.value0, currentValue));
        };
        return discard(when(Data_Maybe.isJust(v.value1))(Dotenv_Internal_Environment.setEnv(v.value0)(Data_Maybe.fromMaybe("")(v.value1))))(function () {
            return pure(new Data_Tuple.Tuple(v.value0, v.value1));
        });
    });
});
export {
    applySettings
};
