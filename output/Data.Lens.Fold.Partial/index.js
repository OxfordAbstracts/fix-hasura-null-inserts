// Generated by purs version 0.15.7
import * as Data_Lens_Fold from "../Data.Lens.Fold/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var unsafeView = function () {
    return function (s) {
        return function (l) {
            return Data_Maybe["fromMaybe$prime"](function (v) {
                return Partial_Unsafe.unsafeCrashWith("unsafeView: Empty fold");
            })(Data_Lens_Fold.previewOn(s)(l));
        };
    };
};
var unsafeIndexedFold = function () {
    return function (s) {
        return function (l) {
            return Data_Maybe["fromMaybe$prime"](function (v) {
                return Partial_Unsafe.unsafeCrashWith("unsafeIndexedFold: empty Fold");
            })(unwrap(Data_Lens_Fold.ifoldMapOf(l)(function (i) {
                return function (a) {
                    return new Data_Maybe.Just(new Data_Tuple.Tuple(i, a));
                };
            })(s)));
        };
    };
};
export {
    unsafeView,
    unsafeIndexedFold
};