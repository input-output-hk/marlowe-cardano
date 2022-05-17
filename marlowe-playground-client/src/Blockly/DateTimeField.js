// This function is analogue to Component.DateTimeLocalInput.State (parseInput)
// It parses the valueStr using the same Regexp and tries to create a Date
// object assuming that the date is in UTC. Functions utcToLocal/localToUtc
// should be used to adjust the offset.
function parseInput(valueStr) {
  const timeRegex = /(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(:(\d{2}))?/;
  const found = timeRegex.exec(valueStr);
  if (found) {
    const date = new Date(`${found[0]}Z`);
    return {
      matchStr: found[0],
      unixTime: date.getTime(), // ms from epoch
    };
  }
  return null;
}

function localToUtc(tzOffset, unixTimeLocal) {
  return unixTimeLocal + tzOffset * 60 * 1000;
}

function utcToLocal(tzOffset, unixTimeUtc) {
  return localToUtc(-tzOffset, unixTimeUtc);
}

function showNormalizedDateTime(unixTime, trimSeconds) {
  const timeRegex = trimSeconds
    ? /(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})/
    : /(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(:(\d{2}))?/;
  const fullStr = new Date(unixTime).toISOString();
  const found = timeRegex.exec(fullStr);
  if (found) return found[0];
  throw "showNormalizedDateTime can normalize a datetime string"; // This should not happen.
}

// Register a custom DateTime Field to blockly:
// Used as base:
// * https://developers.google.com/blockly/guides/create-custom-blocks/fields/customizing-fields/creating
// * https://github.com/google/blockly-samples/blob/master/plugins/field-date/src/field_date.js
export function registerDateTimeField(Blockly) {
  // If the field is already registered, return it.
  const RegisteredFieldDateTime = Blockly.registry
    ? Blockly.registry.getClass("field", "field_datetime")
    : undefined;

  if (RegisteredFieldDateTime) return RegisteredFieldDateTime;

  const FieldDateTime = function (
    tzOffset,
    value = undefined,
    validator = undefined
  ) {
    if (typeof tzOffset != "number") {
      throw new Error(
        "FieldDateTime must be constructed with a numeric timezone offset"
      );
    }
    this.tzOffset = tzOffset;

    // The default value for this field is the current date
    value = this.doClassValidation_(value);

    FieldDateTime.prototype.DEFAULT_VALUE = this.doClassValidation_(
      new Date().getTime()
    );

    FieldDateTime.superClass_.constructor.call(this, value, validator);

    this.onInputWrapper_ = null;
  };
  Blockly.utils.object.inherits(FieldDateTime, Blockly.Field);

  FieldDateTime.fromJson = function (options) {
    return new FieldDateTime(options["date"], undefined);
  };

  FieldDateTime.prototype.SERIALIZABLE = true;

  FieldDateTime.prototype.CURSOR = "text";

  FieldDateTime.prototype.doClassValidation_ = function (newValue = undefined) {
    if (typeof newValue == "number" || typeof newValue == "bigint") {
      return newValue;
    } else if (typeof newValue == "string") {
      const parsedInt = parseInt(newValue, 10);
      if (parsedInt + "" == newValue) {
        return parsedInt;
      }
    }

    return null;
  };

  // The initView is a place in which we can modify how the element shows itself
  FieldDateTime.prototype.initView = function () {
    // Bounding box
    this.createBorderRect_();
    // The picker element is a native HTML DOM input
    // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/datetime-local
    this.picker_ = document.createElement("input");
    this.picker_.type = "datetime-local";
    this.picker_.value = showNormalizedDateTime(
      utcToLocal(this.tzOffset, this.getValue()),
      true
    );

    // In order to show an HTML element in Blockly we need to wrap
    // it on an Svg foreignObject
    const foreignObject = document.createElementNS(
      "http://www.w3.org/2000/svg",
      "foreignObject"
    );
    foreignObject.appendChild(this.picker_);
    this.foreignObject = foreignObject;
    // We add the foreignObject to the fields `g` element.
    this.fieldGroup_.appendChild(foreignObject);
  };

  // In order for the input to be displayed, we need to react
  // to size changes and update the foreignObject dimensions
  // accordingly
  FieldDateTime.prototype.updateSize_ = function () {
    this.size_.width = this.picker_.offsetWidth;
    this.size_.height = this.picker_.offsetHeight;
    this.foreignObject.setAttribute("width", this.picker_.offsetWidth);
    this.foreignObject.setAttribute("height", this.picker_.offsetHeight);
  };

  FieldDateTime.prototype.bindEvents_ = function () {
    FieldDateTime.superClass_.bindEvents_.call(this);
    const thisField = this;

    // Whenever the native input element changes it value, update
    // the field model
    this.onInputWrapper_ = Blockly.bindEventWithChecks_(
      this.picker_,
      "input",
      this,
      function () {
        const parsedValue = parseInput(thisField.picker_.value);
        if (parsedValue) {
          thisField.setValue(
            localToUtc(thisField.tzOffset, parsedValue.unixTime)
          );
        }
      }
    );
  };

  FieldDateTime.prototype.dispose = function () {
    FieldDateTime.superClass_.dispose.call(this);

    if (this.onInputWrapper_) {
      Blockly.unbindEvent_(this.onInputWrapper_);
    }
  };

  Blockly.fieldRegistry.register("field_datetime", FieldDateTime);

  return FieldDateTime;
}
