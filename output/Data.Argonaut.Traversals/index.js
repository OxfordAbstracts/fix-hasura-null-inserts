// Generated by purs version 0.15.7
import * as Data_Argonaut_Core from "../Data.Argonaut.Core/index.js";
import * as Data_Lens_Fold from "../Data.Lens.Fold/index.js";
var _JsonString = function (dictWander) {
    var $20 = Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isString);
    return function ($21) {
        return $20($21);
    };
};
var _JsonObject = function (dictWander) {
    var $22 = Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isObject);
    return function ($23) {
        return $22($23);
    };
};
var _JsonNumber = function (dictWander) {
    var $24 = Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isNumber);
    return function ($25) {
        return $24($25);
    };
};
var _JsonNull = function (dictWander) {
    var $26 = Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isNull);
    return function ($27) {
        return $26($27);
    };
};
var _JsonBoolean = function (dictWander) {
    var $28 = Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isBoolean);
    return function ($29) {
        return $28($29);
    };
};
var _JsonArray = function (dictWander) {
    var $30 = Data_Lens_Fold.filtered(dictWander.Choice1())(Data_Argonaut_Core.isArray);
    return function ($31) {
        return $30($31);
    };
};
export {
    _JsonNull,
    _JsonBoolean,
    _JsonNumber,
    _JsonString,
    _JsonArray,
    _JsonObject
};
