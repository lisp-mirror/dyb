function fetchURI(uri, callback, parameters) {
    var request;
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    else {
        try { request = new ActiveXObject("Msxml2.XMLHTTP"); } catch (e) {
            try { request = new ActiveXObject("Microsoft.XMLHTTP"); } catch (ee) {
                request = null;
            }}}
    if (!request) alert("Browser couldn't make a request object.");

    request.open('POST', uri, true);
    request.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    request.setRequestHeader("Content-length", parameters.length);
    request.setRequestHeader("Connection", "close");

    request.onreadystatechange = function() {
        if (request.readyState != 4) return;
        if (request.status >= 200 && request.status < 300
            || request.status == 304) {
            if (callback)
                callback(request.responseText);
        }
        else {
            alert('Error while fetching URI ' + uri);
        }
    };
    request.send(parameters);
    delete request;
}

function ajax_call(func, callback, args, widget_args) {
    var uri = '/ems/ajax/' + encodeURIComponent(func);
    var post_parameters = '';
    var i;
    if (args.length > 0) {
        uri += '?';
        for (i = 0; i < args.length; ++i) {
            if (i > 0)
                uri += '&';
            uri += 'arg' + i + '=' + encodeURIComponent(args[i]);
        }
    }

    if (widget_args && widget_args.length > 0) {
        for (i = 0; i < widget_args.length; ++i) {
            var arg = widget_args[i]
            if (i > 0)
                post_parameters += '&';
            post_parameters += encodeURIComponent(arg[0]) + '='
                + encodeURIComponent(arg[1]);
        }
    }
    fetchURI(uri, callback, post_parameters);
}

function cl_ajax_render (script_name, widget_id, args, callback) {
    ajax_call('CL-AJAX-RENDER', callback, [script_name, widget_id], args);
}

function find_widget(id) {
    var widget = document.getElementById(id);
    if (!widget) {
        alert("There's no widget with id " + id);
        return;
    }
    return widget;
}

function ajax_render (script_name, widget_id, args) {
    var widget = find_widget(widget_id);

    if (widget) {
        cl_ajax_render(script_name, widget_id, args,
                       function (response) {
                           var json = jQuery.parseJSON(response);
                           widget.innerHTML = json[0];
                           //applyPeach(widget);
                           if (json[1]) {
                               eval(json[1]);
                           }
                       }
                      );
    }
}

function get_values(widget, tag_name) {
    var elements = widget.getElementsByTagName(tag_name);
    var result = [];
    for (var i = 0; i < elements.length; i++) {
        element = elements[i];
        if (element.name)
        {
            
            if (tag_name == 'input')
            {
                if (element.type == 'checkbox')
                {
                    result.push( [element.name, element.checked]);
                }
                else
                {
                    result.push( [element.name, element.value]);
                }
            }
            else
            {
                result.push( [element.name, element.value]);
            }
        }
    }
    return result;
}

function get_form_values(form_id) {
    var widget = find_widget(form_id);
    var input_types = ['input', 'select', 'textarea'];

    if (widget) {
        var result = [];
        for (var i = 0; i < input_types.length; i++)
            result = result.concat(get_values(widget, input_types[i]));
        return result;
    }
}
