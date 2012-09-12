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
