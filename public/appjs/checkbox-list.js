
function checkAll(checkAllBox) {
    var checkboxList = checkAllBox.parentNode.parentNode;
    var checkboxes = checkboxList.getElementsByTagName("input");

    for (var i = 0; i < checkboxes.length; i++) {
        var checkbox = checkboxes[i];

        if (checkbox.type == "checkbox")
	{

            checkbox.checked = checkAllBox.checked;
	}
	if (checkbox.type == "hidden")
	{
		if (checkAllBox.checked)
		{
			checkbox.value = "on";
		}
		else
		{
			checkbox.value = "off";
		}
	}
    }
}

function updateCheckAll(element) {
    var checkboxList = element.parentNode.parentNode.parentNode;
    var checkboxes = checkboxList.getElementsByTagName("input");
    var checkAll = checkboxes[1];
    var allChecked = true;

    for (var i = 2; i < checkboxes.length; i++) {
        var checkbox = checkboxes[i];
        if (checkbox.type == "checkbox"
            && !checkbox.checked) {
            allChecked = false;
            break;
        }
    }
    checkAll.checked = allChecked;
}
