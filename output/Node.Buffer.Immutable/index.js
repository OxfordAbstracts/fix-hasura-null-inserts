// Generated by purs version 0.15.7
import * as $foreign from "./foreign.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Node_Buffer_Types from "../Node.Buffer.Types/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
var toString = function ($7) {
    return $foreign.toStringImpl(Node_Encoding.encodingToNode($7));
};
var showBuffer = {
    show: $foreign.showImpl
};
var readString = function ($8) {
    return $foreign.readStringImpl(Node_Encoding.encodingToNode($8));
};
var read = /* #__PURE__ */ (function () {
    var $9 = Data_Show.show(Node_Buffer_Types.showBufferValueType);
    return function ($10) {
        return $foreign.readImpl($9($10));
    };
})();
var getAtOffset = /* #__PURE__ */ (function () {
    return $foreign.getAtOffsetImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var fromString = function (str) {
    var $11 = $foreign.fromStringImpl(str);
    return function ($12) {
        return $11(Node_Encoding.encodingToNode($12));
    };
};
var eqBuffer = {
    eq: $foreign.eqImpl
};
var ordBuffer = {
    compare: function (a) {
        return function (b) {
            var v = $foreign.compareImpl(a)(b);
            if (v < 0) {
                return Data_Ordering.LT.value;
            };
            if (v > 0) {
                return Data_Ordering.GT.value;
            };
            return Data_Ordering.EQ.value;
        };
    },
    Eq0: function () {
        return eqBuffer;
    }
};
var concat$prime = $foreign.concatToLength;
export {
    create,
    fromArray,
    fromArrayBuffer,
    toArray,
    toArrayBuffer,
    concat,
    slice,
    size
} from "./foreign.js";
export {
    fromString,
    read,
    readString,
    toString,
    getAtOffset,
    concat$prime,
    showBuffer,
    eqBuffer,
    ordBuffer
};