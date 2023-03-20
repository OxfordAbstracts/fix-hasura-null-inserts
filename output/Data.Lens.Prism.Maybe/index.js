// Generated by purs version 0.15.7
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Lens_Prism from "../Data.Lens.Prism/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var _Nothing = function (dictChoice) {
    return Data_Lens_Prism.prism(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.maybe(new Data_Either.Right(Data_Unit.unit))(Data_Function["const"](new Data_Either.Left(Data_Maybe.Nothing.value))))(dictChoice);
};
var _Just = function (dictChoice) {
    return Data_Lens_Prism.prism(Data_Maybe.Just.create)(Data_Maybe.maybe(new Data_Either.Left(Data_Maybe.Nothing.value))(Data_Either.Right.create))(dictChoice);
};
export {
    _Nothing,
    _Just
};
