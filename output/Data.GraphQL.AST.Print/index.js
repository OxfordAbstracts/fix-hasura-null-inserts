// Generated by purs version 0.15.7
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_GraphQL_AST from "../Data.GraphQL.AST/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Show from "../Data.Show/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var show2 = /* #__PURE__ */ Data_Show.show(Data_Show.showNumber);
var show3 = /* #__PURE__ */ Data_Show.show(Data_Show.showBoolean);
var intercalate = /* #__PURE__ */ Data_Foldable.intercalate(Data_List_Types.foldableList)(Data_Monoid.monoidString);
var map = /* #__PURE__ */ Data_Functor.map(Data_List_Types.functorList);
var printAstTypeSystemDirecti = {
    printAst: /* #__PURE__ */ Data_Show.show(Data_GraphQL_AST.typeSystemDirectiveLocationShow)
};
var printAstStringValue = {
    printAst: function (v) {
        return show(v);
    }
};
var printAstString = {
    printAst: function (a) {
        return a;
    }
};
var printAstOperationType = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.Query) {
            return "query";
        };
        if (v instanceof Data_GraphQL_AST.Mutation) {
            return "mutation";
        };
        if (v instanceof Data_GraphQL_AST.Subscription) {
            return "subscription";
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 307, column 14 - line 310, column 39): " + [ v.constructor.name ]);
    }
};
var printAstNullValue = {
    printAst: function (v) {
        return "null";
    }
};
var printAstNamedType = {
    printAst: function (v) {
        return v;
    }
};
var printAstIntValue = {
    printAst: function (v) {
        return show1(v);
    }
};
var printAstFloatValue = {
    printAst: function (v) {
        return show2(v);
    }
};
var printAstExecutableDirecti = {
    printAst: /* #__PURE__ */ Data_Show.show(Data_GraphQL_AST.executableDirectiveLocationShow)
};
var printAstEnumValue = {
    printAst: function (v) {
        return v;
    }
};
var printAstBooleanValue = {
    printAst: function (v) {
        return show3(v);
    }
};
var tripleQuote = function (s) {
    return "\"\"\"" + (s + ("\"\"\"" + "\x0a"));
};
var printExtension = function (s) {
    return "extend " + (s + " ");
};
var printDescription = function (v) {
    if (v instanceof Data_Maybe.Just && v.value0 === "") {
        return "";
    };
    return Data_Maybe.maybe("")(tripleQuote)(v);
};
var printAst = function (dict) {
    return dict.printAst;
};
var printAst1 = /* #__PURE__ */ printAst(printAstTypeSystemDirecti);
var printAst2 = /* #__PURE__ */ printAst(printAstExecutableDirecti);
var printAst3 = /* #__PURE__ */ printAst(printAstNamedType);
var printAst4 = /* #__PURE__ */ printAst(printAstOperationType);
var printAst5 = /* #__PURE__ */ printAst(printAstString);
var printAst6 = /* #__PURE__ */ printAst(printAstIntValue);
var printAst7 = /* #__PURE__ */ printAst(printAstFloatValue);
var printAst8 = /* #__PURE__ */ printAst(printAstStringValue);
var printAst9 = /* #__PURE__ */ printAst(printAstBooleanValue);
var printAst10 = /* #__PURE__ */ printAst(printAstNullValue);
var printAst11 = /* #__PURE__ */ printAst(printAstEnumValue);
var printAstDefaultValue = {
    printAst: function (a) {
        return "=" + printAst(printAstDefaultValue)(a);
    }
};
var printAstDirectiveLocation = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.DirectiveLocation_TypeSystemDirectiveLocation) {
            return printAst1(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.DirectiveLocation_ExecutableDirectiveLocation) {
            return printAst2(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 261, column 14 - line 263, column 70): " + [ v.constructor.name ]);
    }
};
var printAst12 = /* #__PURE__ */ printAst(printAstDirectiveLocation);
var printAstDirectiveLocation1 = {
    printAst: function (v) {
        return intercalate(" | ")(map(printAst12)(v));
    }
};
var printAst13 = /* #__PURE__ */ printAst(printAstDirectiveLocation1);
var printAstImplementsInterfa = {
    printAst: function (v) {
        return "implements " + intercalate(" & ")(map(printAst3)(v));
    }
};
var printAst14 = /* #__PURE__ */ printAst(printAstImplementsInterfa);
var printAstMaybe = function (dictPrintAst) {
    return {
        printAst: Data_Maybe.maybe("")(printAst(dictPrintAst))
    };
};
var printAst15 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstImplementsInterfa));
var printAst16 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstDefaultValue));
var printAst17 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstString));
var printAstOperationTypeDefi = {
    printAst: function (v) {
        return printAst4(v.operationType) + (": " + printAst3(v.namedType));
    }
};
var printAst18 = /* #__PURE__ */ printAst(printAstOperationTypeDefi);
var printAstListOperationType = {
    printAst: function (t) {
        return "{\x0a  " + (intercalate("\x0a  ")(map(printAst18)(t)) + "\x0a}");
    }
};
var printAst19 = /* #__PURE__ */ printAst(printAstListOperationType);
var printAstRootOperationType = {
    printAst: function (v) {
        return printAst4(v.operationType) + (": " + printAst3(v.namedType));
    }
};
var printAst20 = /* #__PURE__ */ printAst(printAstRootOperationType);
var printAstType = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.Type_NamedType) {
            return printAst3(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Type_ListType) {
            return printAst(printAstListType)(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Type_NonNullType) {
            return printAst(printAstNonNullType)(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 392, column 14 - line 395, column 41): " + [ v.constructor.name ]);
    }
};
var printAstNonNullType = {
    printAst: function (t_) {
        return (function () {
            if (t_ instanceof Data_GraphQL_AST.NonNullType_NamedType) {
                return printAst3(t_.value0);
            };
            if (t_ instanceof Data_GraphQL_AST.NonNullType_ListType) {
                return printAst(printAstListType)(t_.value0);
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 402, column 7 - line 404, column 51): " + [ t_.constructor.name ]);
        })() + "! ";
    }
};
var printAstListType = {
    printAst: function (v) {
        return "[" + (printAst(printAstType)(v) + "]");
    }
};
var printAst21 = /* #__PURE__ */ printAst(printAstType);
var printAstTypeCondition = {
    printAst: function (v) {
        return "on " + printAst3(v);
    }
};
var printAst22 = /* #__PURE__ */ printAst(printAstTypeCondition);
var printAst23 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstTypeCondition));
var printAstFragmentDefinitio = {
    printAst: function (v) {
        return "fragment " + printAst22(v.typeCondition);
    }
};
var printAst24 = /* #__PURE__ */ printAst(printAstFragmentDefinitio);
var printAstUnionMemberTypes = {
    printAst: function (v) {
        return intercalate(" | ")(map(printAst3)(v));
    }
};
var printAst25 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstUnionMemberTypes));
var printAst26 = /* #__PURE__ */ printAst(printAstUnionMemberTypes);
var printAstVariable = {
    printAst: function (v) {
        return "$" + printAst5(v);
    }
};
var printAst27 = /* #__PURE__ */ printAst(printAstVariable);
var printAstValue = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.Value_Variable) {
            return printAst27(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_IntValue) {
            return printAst6(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_FloatValue) {
            return printAst7(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_StringValue) {
            return printAst8(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_BooleanValue) {
            return printAst9(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_NullValue) {
            return printAst10(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_EnumValue) {
            return printAst11(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_ListValue) {
            return printAst(printAstListValue)(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Value_ObjectValue) {
            return printAst(printAstObjectValue)(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 410, column 14 - line 419, column 42): " + [ v.constructor.name ]);
    }
};
var printAstObjectValue = {
    printAst: function (v) {
        return "{ " + (intercalate(" ")(map(printAst(printAstArgument))(v)) + " }");
    }
};
var printAstListValue = {
    printAst: function (v) {
        return "[" + (intercalate(" ")(map(printAst(printAstValue))(v)) + "]");
    }
};
var printAstArgument = {
    printAst: function (v) {
        return v.name + (": " + printAst(printAstValue)(v.value));
    }
};
var printAst28 = /* #__PURE__ */ printAst(printAstArgument);
var printAstArguments = {
    printAst: function (v) {
        return "(" + (intercalate(", ")(map(printAst28)(v)) + ")");
    }
};
var printAst29 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstArguments));
var printAstDirective = {
    printAst: function (v) {
        return "@" + (v.name + printAst29(v["arguments"]));
    }
};
var printAst30 = /* #__PURE__ */ printAst(printAstDirective);
var printAstDirectives = {
    printAst: function (v) {
        return intercalate(" ")(map(printAst30)(v));
    }
};
var printAst31 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstDirectives));
var printAst32 = /* #__PURE__ */ printAst(printAstDirectives);
var printAstEnumValueDefiniti = {
    printAst: function (v) {
        return printDescription(v.description) + (printAst11(v.enumValue) + printAst31(v.directives));
    }
};
var printAst33 = /* #__PURE__ */ printAst(printAstEnumValueDefiniti);
var printAstEnumValuesDefinit = {
    printAst: function (v) {
        return "{\x0a  " + (intercalate("\x0a  ")(map(printAst33)(v)) + "\x0a}");
    }
};
var printAst34 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstEnumValuesDefinit));
var printAst35 = /* #__PURE__ */ printAst(printAstEnumValuesDefinit);
var printAstEnumTypeDefinitio = {
    printAst: function (v) {
        return printDescription(v.description) + ("enum " + (printAst5(v.name) + (" " + (printAst31(v.directives) + printAst34(v.enumValuesDefinition)))));
    }
};
var printAst36 = /* #__PURE__ */ printAst(printAstEnumTypeDefinitio);
var printAstEnumTypeExtension = {
    printAst: function (t) {
        return printExtension("enum") + (function () {
            if (t instanceof Data_GraphQL_AST.EnumTypeExtension_With_Directives) {
                return printAst5(t.value0.name) + (" " + printAst32(t.value0.directives));
            };
            if (t instanceof Data_GraphQL_AST.EnumTypeExtension_With_EnumValuesDefinition) {
                return printAst5(t.value0.name) + (" " + (printAst31(t.value0.directives) + printAst35(t.value0.enumValuesDefinition)));
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 186, column 41 - line 195, column 41): " + [ t.constructor.name ]);
        })();
    }
};
var printAst37 = /* #__PURE__ */ printAst(printAstEnumTypeExtension);
var printAstFragmentSpread = {
    printAst: function (v) {
        return "..." + (printAst5(v.fragmentName) + printAst31(v.directives));
    }
};
var printAst38 = /* #__PURE__ */ printAst(printAstFragmentSpread);
var printAstInputValueDefinit = {
    printAst: function (v) {
        return printDescription(v.description) + (printAst5(v.name) + (": " + (printAst21(v.type) + printAst31(v.directives))));
    }
};
var printAst39 = /* #__PURE__ */ printAst(printAstInputValueDefinit);
var printAstArgumentsDefiniti = {
    printAst: function (v) {
        return "(" + (intercalate(", ")(map(printAst39)(v)) + ")");
    }
};
var printAst40 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstArgumentsDefiniti));
var printAstDirectiveDefiniti = {
    printAst: function (v) {
        return printDescription(v.description) + ("directive @" + (printAst5(v.name) + (" " + (printAst40(v.argumentsDefinition) + (" on " + printAst13(v.directiveLocations))))));
    }
};
var printAst41 = /* #__PURE__ */ printAst(printAstDirectiveDefiniti);
var printAstFieldDefinition = {
    printAst: function (v) {
        return printDescription(v.description) + (" " + (printAst5(v.name) + (printAst40(v.argumentsDefinition) + (": " + (printAst21(v.type) + printAst31(v.directives))))));
    }
};
var printAst42 = /* #__PURE__ */ printAst(printAstFieldDefinition);
var printAstFieldsDefinition = {
    printAst: function (v) {
        return "{\x0a  " + (intercalate("\x0a  ")(map(printAst42)(v)) + "\x0a}");
    }
};
var printAst43 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstFieldsDefinition));
var printAst44 = /* #__PURE__ */ printAst(printAstFieldsDefinition);
var printAstInputFieldsDefini = {
    printAst: function (v) {
        return "{\x0a  " + (intercalate("\x0a  ")(map(printAst39)(v)) + "\x0a}");
    }
};
var printAst45 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstInputFieldsDefini));
var printAst46 = /* #__PURE__ */ printAst(printAstInputFieldsDefini);
var printAstInputObjectTypeDe = {
    printAst: function (v) {
        return printDescription(v.description) + ("input " + (printAst5(v.name) + (" " + (printAst31(v.directives) + printAst45(v.inputFieldsDefinition)))));
    }
};
var printAst47 = /* #__PURE__ */ printAst(printAstInputObjectTypeDe);
var printAstInputObjectTypeEx = {
    printAst: function (t) {
        return printExtension("input") + (function () {
            if (t instanceof Data_GraphQL_AST.InputObjectTypeExtension_With_Directives) {
                return printAst5(t.value0.name) + (" " + printAst32(t.value0.directives));
            };
            if (t instanceof Data_GraphQL_AST.InputObjectTypeExtension_With_InputFieldsDefinition) {
                return printAst5(t.value0.name) + (" " + (printAst31(t.value0.directives) + printAst46(t.value0.inputFieldsDefinition)));
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 174, column 42 - line 183, column 42): " + [ t.constructor.name ]);
        })();
    }
};
var printAst48 = /* #__PURE__ */ printAst(printAstInputObjectTypeEx);
var printAstInterfaceTypeDefi = {
    printAst: function (v) {
        return printDescription(v.description) + ("interface " + (printAst5(v.name) + (" " + (printAst31(v.directives) + printAst43(v.fieldsDefinition)))));
    }
};
var printAst49 = /* #__PURE__ */ printAst(printAstInterfaceTypeDefi);
var printAstInterfaceTypeExte = {
    printAst: function (t) {
        return printExtension("input") + (function () {
            if (t instanceof Data_GraphQL_AST.InterfaceTypeExtension_With_FieldsDefinition) {
                return printAst5(t.value0.name) + (" " + (printAst31(t.value0.directives) + printAst44(t.value0.fieldsDefinition)));
            };
            if (t instanceof Data_GraphQL_AST.InterfaceTypeExtension_With_Directives) {
                return printAst5(t.value0.name) + (" " + printAst32(t.value0.directives));
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 198, column 42 - line 207, column 31): " + [ t.constructor.name ]);
        })();
    }
};
var printAst50 = /* #__PURE__ */ printAst(printAstInterfaceTypeExte);
var printAstObjectTypeDefinit = {
    printAst: function (v) {
        return printDescription(v.description) + ("type " + (printAst5(v.name) + (" " + (printAst15(v.implementsInterfaces) + (printAst31(v.directives) + printAst43(v.fieldsDefinition))))));
    }
};
var printAst51 = /* #__PURE__ */ printAst(printAstObjectTypeDefinit);
var printAstObjectTypeExtensi = {
    printAst: function (t) {
        return printExtension("type") + (function () {
            if (t instanceof Data_GraphQL_AST.ObjectTypeExtension_With_ImplementsInterfaces) {
                return printAst5(t.value0.name) + (" " + printAst14(t.value0.implementsInterfaces));
            };
            if (t instanceof Data_GraphQL_AST.ObjectTypeExtension_With_Directives) {
                return printAst5(t.value0.name) + (" " + (printAst15(t.value0.implementsInterfaces) + printAst32(t.value0.directives)));
            };
            if (t instanceof Data_GraphQL_AST.ObjectTypeExtension_With_FieldsDefinition) {
                return printAst5(t.value0.name) + (" " + (printAst15(t.value0.implementsInterfaces) + (printAst31(t.value0.directives) + printAst44(t.value0.fieldsDefinition))));
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 223, column 41 - line 238, column 37): " + [ t.constructor.name ]);
        })();
    }
};
var printAst52 = /* #__PURE__ */ printAst(printAstObjectTypeExtensi);
var printAstScalarTypeDefinit = {
    printAst: function (v) {
        return printDescription(v.description) + ("scalar " + (printAst5(v.name) + (" " + printAst31(v.directives))));
    }
};
var printAst53 = /* #__PURE__ */ printAst(printAstScalarTypeDefinit);
var printAstScalarTypeExtensi = {
    printAst: function (v) {
        return "scalar " + (printAst5(v.name) + (" " + printAst32(v.directives)));
    }
};
var printAst54 = /* #__PURE__ */ printAst(printAstScalarTypeExtensi);
var printAstSchemaDefinition = {
    printAst: function (v) {
        return "schema " + (printAst31(v.directives) + (" " + ("{\x0a  " + (intercalate("\x0a  ")(map(printAst20)(v.rootOperationTypeDefinition)) + "\x0a}"))));
    }
};
var printAst55 = /* #__PURE__ */ printAst(printAstSchemaDefinition);
var printAstSchemaExtension = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.SchemaExtension_With_OperationTypeDefinition) {
            return printAst31(v.value0.directives) + (" " + printAst19(v.value0.operationTypesDefinition));
        };
        if (v instanceof Data_GraphQL_AST.SchemaExtension_With_Directives) {
            return printAst32(v.value0.directives);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 41, column 14 - line 49, column 26): " + [ v.constructor.name ]);
    }
};
var printAst56 = /* #__PURE__ */ printAst(printAstSchemaExtension);
var printAstUnionTypeDefiniti = {
    printAst: function (v) {
        return printDescription(v.description) + ("union " + (printAst5(v.name) + (" " + (printAst31(v.directives) + ("= " + printAst25(v.unionMemberTypes))))));
    }
};
var printAst57 = /* #__PURE__ */ printAst(printAstUnionTypeDefiniti);
var printAstTypeDefinition = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.TypeDefinition_ScalarTypeDefinition) {
            return printAst53(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeDefinition_ObjectTypeDefinition) {
            return printAst51(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeDefinition_InterfaceTypeDefinition) {
            return printAst49(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeDefinition_UnionTypeDefinition) {
            return printAst57(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeDefinition_EnumTypeDefinition) {
            return printAst36(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeDefinition_InputObjectTypeDefinition) {
            return printAst47(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 81, column 14 - line 87, column 65): " + [ v.constructor.name ]);
    }
};
var printAst58 = /* #__PURE__ */ printAst(printAstTypeDefinition);
var printAstTypeSystemDefinit = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.TypeSystemDefinition_SchemaDefinition) {
            return printAst55(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeSystemDefinition_TypeDefinition) {
            return printAst58(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeSystemDefinition_DirectiveDefinition) {
            return printAst41(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 30, column 14 - line 33, column 65): " + [ v.constructor.name ]);
    }
};
var printAst59 = /* #__PURE__ */ printAst(printAstTypeSystemDefinit);
var printAstUnionTypeExtensio = {
    printAst: function (t) {
        return printExtension("union") + (function () {
            if (t instanceof Data_GraphQL_AST.UnionTypeExtension_With_Directives) {
                return printAst5(t.value0.name) + (" " + printAst32(t.value0.directives));
            };
            if (t instanceof Data_GraphQL_AST.UnionTypeExtension_With_UnionMemberTypes) {
                return printAst5(t.value0.name) + (" " + (printAst31(t.value0.directives) + ("= " + printAst26(t.value0.unionMemberTypes))));
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 210, column 42 - line 220, column 37): " + [ t.constructor.name ]);
        })();
    }
};
var printAst60 = /* #__PURE__ */ printAst(printAstUnionTypeExtensio);
var printAstTypeExtension = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.TypeExtension_ScalarTypeExtension) {
            return printAst54(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeExtension_ObjectTypeExtension) {
            return printAst52(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeExtension_InterfaceTypeExtension) {
            return printAst50(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeExtension_UnionTypeExtension) {
            return printAst60(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeExtension_EnumTypeExtension) {
            return printAst37(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeExtension_InputObjectTypeExtension) {
            return printAst48(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 155, column 14 - line 161, column 63): " + [ v.constructor.name ]);
    }
};
var printAst61 = /* #__PURE__ */ printAst(printAstTypeExtension);
var printAstTypeSystemExtensi = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.TypeSystemExtension_SchemaExtension) {
            return printAst56(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.TypeSystemExtension_TypeExtension) {
            return printAst61(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 36, column 14 - line 38, column 58): " + [ v.constructor.name ]);
    }
};
var printAst62 = /* #__PURE__ */ printAst(printAstTypeSystemExtensi);
var printAstSelectionSet = {
    printAst: function (v) {
        return "{\x0a  " + (intercalate("\x0a  ")(map(printAst(printAstSelection))(v)) + "\x0a}\x0a");
    }
};
var printAstSelection = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.Selection_Field) {
            return printAst(printAstField)(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Selection_FragmentSpread) {
            return printAst38(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Selection_InlineFragment) {
            return printAst(printAstInlineFragment)(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 325, column 14 - line 328, column 49): " + [ v.constructor.name ]);
    }
};
var printAstInlineFragment = {
    printAst: function (v) {
        return "..." + (printAst23(v.typeCondition) + (printAst31(v.directives) + printAst(printAstSelectionSet)(v.selectionSet)));
    }
};
var printAstField = {
    printAst: function (v) {
        var printAlias = function (v1) {
            if (v1 instanceof Data_Maybe.Just) {
                return v1.value0 + ": ";
            };
            if (v1 instanceof Data_Maybe.Nothing) {
                return "";
            };
            throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 343, column 18 - line 345, column 20): " + [ v1.constructor.name ]);
        };
        return printAlias(v.alias) + (" " + (printAst5(v.name) + (" " + (printAst29(v["arguments"]) + (" " + (printAst31(v.directives) + (" " + printAst(printAstMaybe(printAstSelectionSet))(v.selectionSet))))))));
    }
};
var printAst63 = /* #__PURE__ */ printAst(printAstSelectionSet);
var printAstVariableDefinitio = {
    printAst: function (v) {
        return printAst27(v.variable) + (": " + (printAst21(v.type) + printAst16(v.defaultValue)));
    }
};
var printAst64 = /* #__PURE__ */ printAst(printAstVariableDefinitio);
var printAstVariableDefinitio1 = {
    printAst: function (v) {
        return "(" + (intercalate(" ")(map(printAst64)(v)) + ")");
    }
};
var printAst65 = /* #__PURE__ */ printAst(/* #__PURE__ */ printAstMaybe(printAstVariableDefinitio1));
var printAstOperationDefiniti = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.OperationDefinition_SelectionSet) {
            return printAst63(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.OperationDefinition_OperationType) {
            return printAst4(v.value0.operationType) + (" " + (printAst17(v.value0.name) + (" " + (printAst65(v.value0.variableDefinitions) + (" " + (printAst31(v.value0.directives) + (" " + printAst63(v.value0.selectionSet))))))));
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 61, column 14 - line 64, column 154): " + [ v.constructor.name ]);
    }
};
var printAst66 = /* #__PURE__ */ printAst(printAstOperationDefiniti);
var printAstExecutableDefinit = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.ExecutableDefinition_OperationDefinition) {
            return printAst66(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.ExecutableDefinition_FragmentDefinition) {
            return printAst24(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 25, column 14 - line 27, column 64): " + [ v.constructor.name ]);
    }
};
var printAst67 = /* #__PURE__ */ printAst(printAstExecutableDefinit);
var printAstDefinition = {
    printAst: function (v) {
        if (v instanceof Data_GraphQL_AST.Definition_ExecutableDefinition) {
            return printAst67(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Definition_TypeSystemDefinition) {
            return printAst59(v.value0);
        };
        if (v instanceof Data_GraphQL_AST.Definition_TypeSystemExtension) {
            return printAst62(v.value0);
        };
        throw new Error("Failed pattern match at Data.GraphQL.AST.Print (line 19, column 14 - line 22, column 55): " + [ v.constructor.name ]);
    }
};
var printAst68 = /* #__PURE__ */ printAst(printAstDefinition);
var printAstDocument = {
    printAst: function (v) {
        return intercalate("\x0a\x0a")(map(printAst68)(v));
    }
};
export {
    printAst,
    printExtension,
    printDescription,
    tripleQuote,
    printAstDocument,
    printAstDefinition,
    printAstExecutableDefinit,
    printAstTypeSystemDefinit,
    printAstTypeSystemExtensi,
    printAstSchemaExtension,
    printAstListOperationType,
    printAstOperationTypeDefi,
    printAstOperationDefiniti,
    printAstSchemaDefinition,
    printAstRootOperationType,
    printAstTypeDefinition,
    printAstScalarTypeDefinit,
    printAstObjectTypeDefinit,
    printAstInputObjectTypeDe,
    printAstInterfaceTypeDefi,
    printAstUnionTypeDefiniti,
    printAstEnumTypeDefinitio,
    printAstDirectiveDefiniti,
    printAstTypeExtension,
    printAstScalarTypeExtensi,
    printAstInputObjectTypeEx,
    printAstEnumTypeExtension,
    printAstInterfaceTypeExte,
    printAstUnionTypeExtensio,
    printAstObjectTypeExtensi,
    printAstImplementsInterfa,
    printAstInputFieldsDefini,
    printAstEnumValuesDefinit,
    printAstDirectiveLocation1,
    printAstUnionMemberTypes,
    printAstDirectiveLocation,
    printAstTypeSystemDirecti,
    printAstExecutableDirecti,
    printAstEnumValueDefiniti,
    printAstEnumValue,
    printAstFieldsDefinition,
    printAstFieldDefinition,
    printAstArgumentsDefiniti,
    printAstInputValueDefinit,
    printAstOperationType,
    printAstFragmentDefinitio,
    printAstTypeCondition,
    printAstSelectionSet,
    printAstSelection,
    printAstField,
    printAstFragmentSpread,
    printAstInlineFragment,
    printAstVariableDefinitio1,
    printAstDirectives,
    printAstDirective,
    printAstArguments,
    printAstArgument,
    printAstVariableDefinitio,
    printAstDefaultValue,
    printAstType,
    printAstListType,
    printAstNonNullType,
    printAstValue,
    printAstVariable,
    printAstIntValue,
    printAstFloatValue,
    printAstStringValue,
    printAstBooleanValue,
    printAstListValue,
    printAstObjectValue,
    printAstNullValue,
    printAstNamedType,
    printAstMaybe,
    printAstString
};
