var afterLoad = [];

function arrayPosition(array, string) {
    for (var i = 0; i < array.length; ++i) {
        if (array[i] === string)
            return i;
    }
    return -1;
}

window.onload = function ()
{
    for (var i = 0; i < afterLoad.length; i++)
        afterLoad[i]();
}
var site_url = "http://dxw.co.za/";

function shortifyString (string) {
    // Should be synchronized with lisp shortify-string 
    return string.replace(RegExp('(https://|http://|www\\.)\\S+'),
                          function (match) {
                              var last = match[match.length - 1];
                              var suffix = "";
                              // For http://en.wikipedia.org/wiki/Egypt_(bird)
                              if (last == ')') {
                                  if (match.indexOf('(') < 0)
                                      suffix = last;
                              } else if (",.(:;".indexOf(last) >= 0) {
                                  suffix = last;
                              }

                              return site_url + "s/xxxx" + suffix;
                          }
                         )
}
