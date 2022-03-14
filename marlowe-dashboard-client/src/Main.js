"use strict";

window.transcript =
  process.env.NODE_ENV === "development" && localStorage.getItem("transcribing")
    ? []
    : undefined;

window.startTranscribing =
  process.env.NODE_ENV === "development"
    ? () => {
        localStorage.setItem("transcribing", "true");
        window.transcript = [];
      }
    : undefined;

window.stopTranscribing =
  process.env.NODE_ENV === "development"
    ? () => {
        localStorage.removeItem("transcribing");
        window.transcript = undefined;
      }
    : undefined;

exports.setShowTranscript = (showTranscript) => () => {
  window.showTranscript =
    process.env.NODE_ENV === "development"
      ? (name) => {
          if (window.transcript) {
            const lines = [
              "module Test.Transcript." + name + " where",
              "import Prologue",
              "",
              "import Data.Argonaut.Extra (parseDecodeJson, unsafeParseJson)",
              "import Data.Time.Duration (Milliseconds(..))",
              "import Transcript (Transcript(..), TranscriptEvent(..))",
              "",
              "transcript :: Transcript",
              "transcript = " + showTranscript(window.transcript),
            ];
            console.log(lines.join("\n"));
          }
        }
      : undefined;
};

exports.transcribe = (event) => () => {
  if (process.env.NODE_ENV === "development" && window.transcript) {
    window.transcript.push(event);
  }
};
