"use strict";
// This Function starts reading a stream, discarding characters until
// it reaches the begin separator, then it reads until it reaches the
// end separator. If the inner string can be parsed as Json, it fires
// an onJson event.
exports._createJsonStream = function ({
  stream,
  slizeSize,
  beginSeparator,
  endSeparator,
  onJson,
  onFinish,
  onError,
  streamError,
  invalidJson,
}) {
  return () => {
    stream.setEncoding("utf8");
    let jsonBuffer = Buffer.alloc(slizeSize);
    let jsonIndex = 0;
    let inJson = false;

    function resizeBuffer(minSize) {
      jsonBuffer = Buffer.concat([
        jsonBuffer,
        Buffer.alloc(Math.max(minSize, slizeSize)),
      ]);
    }

    stream.on("close", () => onFinish());
    stream.on("error", () => onError(streamError)());
    stream.on("data", (chunk) => {
      let i = 0;
      while (i < chunk.length) {
        // If we are not in Json mode then we discard characters until we see the beginSeparator
        if (!inJson) {
          if (
            i + beginSeparator.length > chunk.length ||
            chunk.toString().substring(i, i + beginSeparator.length) !=
              beginSeparator
          ) {
            i++;
          } else {
            inJson = true;
            i += beginSeparator.length;
          }
        }

        // If we are in Json mode, we need to see if we are ending the sequence
        // or if we are accumulating chars into the jsonBuffer (with possible resize)
        if (inJson) {
          if (
            i + endSeparator.length < chunk.length &&
            chunk.toString().substring(i, i + endSeparator.length) ==
              endSeparator
          ) {
            const jsonString = jsonBuffer.toString().substring(0, jsonIndex);

            let json;
            try {
              json = JSON.parse(jsonString);
            } catch (err) {}
            if (typeof json === "undefined") {
              onError(invalidJson)();
            } else {
              onJson(json)();
              i += endSeparator.length;
            }
            inJson = false;
            jsonIndex = 0;
          } else {
            // If we are here, we are in json mode and we need to accumulate
            // the current char.

            // First we need to see if we need to resize the buffer at least
            // the rest of the chunk size.
            if (jsonIndex >= jsonBuffer.length) {
              resizeBuffer(chunk.length - i);
            }
            jsonBuffer.write(chunk[i], jsonIndex, 1, "utf8");
            jsonIndex++;
            i++;
          }
        }
      }
    });
  };
};
