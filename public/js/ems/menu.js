function getCookie (name)
{
    var cookie = document.cookie;
    var start = cookie.indexOf(name + "=");

    if (start >= 0) {
        var end = cookie.indexOf(";", start);
        start += name.length + 1;
        if (end <= 0)
            end = cookie.length;
        return cookie.substring(start, end);
    }
}

function setCookie (name, value)
{
    document.cookie = name + "=" + value + ";path=/"
}

function getExpanded()
{
    var expanded = getCookie("expanded");

    if (expanded)
        return expanded.split(":");
    else
        return []
}

function setExpanded(array)
{
    var string = "";
    for (var i = 0; i < array.length; i++) {
        string += array[i];
        if (i < array.length -1)
            string += ":";
    }

    setCookie("expanded", string)
}

function addExpanded(id)
{
    var expanded = getExpanded()
    if (arrayPosition(expanded, id.toString()) < 0)
        expanded.push(id)
    setExpanded(expanded)
}

function removeExpanded(id)
{
    var expanded = getExpanded()
    var position = arrayPosition(expanded, id.toString())
    if (position >= 0) {
        expanded.splice(position, 1)
        setExpanded(expanded)
    }
}

function makeMenuLink(description, linkId)
{
    var url = description[0];
    var image = description[1];
    var title = description[2];

    if (url)
        url = "/ems/" + url;
    else
        url = "#";

    var li = document.createElement("li");
    if (image) {
        image = "<img src='/images/" + image + "'>";
    } else {
        image = "";
    }

    if (url == window.location.pathname) {
        title = "<span class='selected'>" + title + "</span>";
    }

    li.innerHTML = "<a href='" + url + "'>" + image + title;
    var a = li.getElementsByTagName("a")[0];

    return li;
}

function makeTreeSwitcher(li, ul, linkId)
{
    var span = document.createElement("span");
    var link = li.getElementsByTagName("a")[0];

    li.appendChild(span)
    span.className = (ul.style.display == "none") ? "collapsed" : "expanded";

    span.onclick = function() {
        if (ul.style.display == "none") {
            ul.style.display = "block";
            span.className = "expanded";
            addExpanded(linkId);
        } else {
            ul.style.display = "none";
            span.className = "collapsed";
            removeExpanded(linkId)
        }
    };
    link.onclick = function () {span.onclick(); return false;}
}

function layMenu(tree, topUl, linkId)
{
    var expanded = getExpanded();

    for (var i = 0; i < tree.length; i++)
    {
        var element = tree[i];
        if (element[0] instanceof Array) {
            var li = makeMenuLink(element.shift(), linkId);
            topUl.appendChild(li);
            var ul = document.createElement("ul");
            if (arrayPosition(expanded, linkId.toString()) >= 0) {
                ul.style.display = "block";
            } else {
                ul.style.display = "none";
            }
            li.appendChild(ul);
            makeTreeSwitcher(li, ul, linkId++);

            linkId = layMenu(element, ul, linkId);
        } else {
            topUl.appendChild(makeMenuLink(element, linkId++));
        }
    }
    return linkId;
}

function addMenu(tree)
{
    var topUl = document.getElementById("site-menu");
    layMenu(tree, topUl, 0);
}

function expandAll() {
    toggleAll("collapsed")
}

function collapseAll() {
    toggleAll("expanded")
}

function toggleAll(state) {
    var topUl = document.getElementById("site-menu");
    var spans = topUl.getElementsByTagName("span")

    for (var i = 0; i < spans.length; i++) {
        var span = spans[i];
        if (span.className == state)
            span.onclick();
    }
}
