// Provide color coding to BOX elements for Ampersand interface based on status value
// Concept must have a relation to a Status concept
// Value for status must be one of the following specified colors


$(".AtomName:contains('Green')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "green").find("div").css("color", "white");
$(".AtomName:contains('Red')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "red").find("div").css("color", "white");
$(".AtomName:contains('Yellow')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "yellow").find("div").css("color", "black");
$(".AtomName:contains('Orange')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "orange").find("div").css("color", "black");
$(".AtomName:contains('White')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "white").find("div").css("color", "black");
$(".AtomName:contains('Black')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "black").find("div").css("color", "white");
$(".AtomName:contains('Grey')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "grey").find("div").css("color", "white");
$(".AtomName:contains('Blue')").closest(".AtomList[concept='Status']").closest(".InterfaceList").css("background-color", "blue").find("div").css("color", "white");

// Hide status elementen
$(".AtomName:contains('Green'), .AtomName:contains('Red'), .AtomName:contains('Yellow'), .AtomName:contains('Orange'), .AtomName:contains('White'), .AtomName:contains('Black'), .AtomName:contains('Grey'), .AtomName:contains('Blue')").closest(".AtomList[concept='Status']").closest(".Interface").hide();