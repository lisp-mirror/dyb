function submitForm(formName, validationSet)
{
    if (validationSet.length)
    {
        var form = document.getElementsByName(formName)[0];
        return validateForm(form, validationSet);
    }

    return true;
}

function validateForm(formObject, validationSet)
{
    var passed = true;
    for (var i = 0; i < validationSet.length; i++)
    {
        var validationObject = validationSet[i];
        var element = getFormElement(formObject, validationObject.element);
        var valElement = getElement("validate-" + validationObject.element);

        if (!element)
            return false;

        var type = validationObject.type;
        var required = validationObject.required;

        if (required && !validateElement(element, valElement, "required")) {
            passed = false;
        } else if (!validateElement(element, valElement, type)) {
            passed = false;
        }
    }
    displayLegend(!passed);
    return passed;
}

function validateElement(element,valElement, type) {
    var validators = {required: requiredValidtor,
                      email: emailValidator,
                      date: dateValidator,
                      numeric: numericValidator,
                      text: function(){return true;}};

    var colors = {normal: "#FFFFFF",
                  required: "#FF0000",
                  email: "#FFCC00",
                  date: "#FFFF00",
                  numeric: "pink"};

    var validator = validators[type];
    var value = elementValue(element);

    if (!validator) {
        alert("Unkown validation type: " + type);
        return false;
    }

    if ((!value && type != "required")
        || validator(value)) {
        element.style.backgroundColor = colors.normal;
        if (valElement)
            valElement.style.display = "none";
        return true;
    } else {
        element.style.backgroundColor = colors[type];
        if (valElement)
            valElement.style.display = "block";
        return false;
    }
}

function displayLegend(enable) {
    var legendDiv = document.getElementById("legend-div");

    if(enable)
        legendDiv.style.display = "block";
    else
        legendDiv.style.display = "none";
}

function getFormElement(formObject, elementName) {
    var element = formObject.elements[elementName];

    if(element == undefined)
    {
        alert("Element does not exist to validate: " + elementName);
        return false;
    }

    if(element.style == undefined)
    {
        alert("Style does not exist for: " + elementName);
        return false;
    }

    return element;
}

function getElement(elementName) {
    var element = document.getElementById(elementName);

    return element;
}

function selectedValue(select)
{
    return select.options[select.selectedIndex].value;
}

function elementValue(element) {
    switch(element.type)
    {
    case "text":
    case "textarea":
        {
            return element.value;
        }
    case "select":
        {
            return selectedValue(element);
        }
    }
}

function emailValidator (value) {
    var splitted = value.match("^(.+)@(.+)$");
    if(!splitted)
        return false;

    if(splitted[1])
    {
        var regexp_user=/^\"?[\w-_\.]*\"?$/;
        if(!splitted[1].match(regexp_user))
            return false;
    }

    if(splitted[2])
    {
        var regexp_domain=/^[\w-\.]*\.[A-Za-z]{2,4}$/;
        if(!splitted[2].match(regexp_domain))
        {
            var regexp_ip =/^\[\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\]$/;
            if(!splitted[2].match(regexp_ip))
                return false;
        }
        return true;
    }
    return false;
}

var months = [false, "january", "february", "march", "april",
              "may", "june", "july", "august", "september",
              "october", "november", "december"];

var months_short = [false, "jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "aug", "sep", "oct", "nov", "dec"];

var days = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

function monthNumber(month) {
    var lowerCased = month.toLowerCase();
    var position = arrayPosition(months, lowerCased)
    if (position >= 0) {
        return position
    } else {
        position = arrayPosition(months_short, lowerCased);
        if (position >= 0)
            return position
    }
}

function daysInMonth(month, year) {
    var monthx = monthNumber(month);
    if (monthx) {
        var day = days[monthx];
        if (day == 28 && isLeap(year))
            return 29;
        else
            return day;
    }
}

function isLeap (year)
{
    return Boolean(!(year % 4) && (!(year % 400) || year % 100));
}

function dateValidator(value) {
    var date = value.match(/^(\d{1,2})\s+(\w+)\s+(\d{4})$/);
    if (!date) return false;

    var day = date[1];
    var month = date[2];
    var year = date[3];
    var maxDays = daysInMonth(month, year);

    return year > 999 && maxDays && day > 0 && day <= maxDays;
}

function numericValidator(value)
{
    if (value != null && !value.toString().match(/^[-]?\d*\.?\d*$/)) return false;
    return true;
    //return value.match("^[0-9]+$");
}

function requiredValidtor(value) {
    // trim spaces
    return Boolean(value.replace(/^\s+|\s+$/g, ""));
}
