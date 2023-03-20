// Generated by purs version 0.15.7
import * as Data_GraphQL_AST from "../Data.GraphQL.AST/index.js";
import * as Data_Lens_Prism from "../Data.Lens.Prism/index.js";
import * as Data_Lens_Traversal from "../Data.Lens.Traversal/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var traversed = /* #__PURE__ */ Data_Lens_Traversal.traversed(Data_List_Types.traversableList);
var uPrism = /* #__PURE__ */ Data_Tuple.uncurry(Data_Lens_Prism["prism$prime"]);
var inputObjectTypeDefinitionLens = function (dictChoice) {
    return function (dictWander) {
        var $11 = uPrism(Data_GraphQL_AST["_Document"])(dictChoice);
        var $12 = traversed(dictWander);
        var $13 = uPrism(Data_GraphQL_AST["_Definition_TypeSystemDefinition"])(dictChoice);
        var $14 = uPrism(Data_GraphQL_AST["_TypeSystemDefinition_TypeDefinition"])(dictChoice);
        var $15 = uPrism(Data_GraphQL_AST["_TypeDefinition_InputObjectTypeDefinition"])(dictChoice);
        return function ($16) {
            return $11($12($13($14($15($16)))));
        };
    };
};
var inputFieldsLens = function (dictTraversable) {
    var traversed1 = Data_Lens_Traversal.traversed(dictTraversable);
    return function (dictWander) {
        var $17 = traversed1(dictWander);
        var $18 = uPrism(Data_GraphQL_AST["_InputFieldsDefinition"])(dictWander.Choice1());
        var $19 = traversed(dictWander);
        return function ($20) {
            return $17($18($19($20)));
        };
    };
};
export {
    inputObjectTypeDefinitionLens,
    inputFieldsLens,
    uPrism
};