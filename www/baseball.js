//Document Key Down--Enter Key Response to Search Button
    $(document).on("keydown", "#search", function(e) {
      if (e.which == 13) {
        $("#search_button").click();
      }
    });

//<!--Code to Display or Hide the Plot-->
function myFunction() {
  var x = document.getElementById("table_one");
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}

//Show Baseball and Pitching Table Outputs once Search Button is Clicked
function myFunction2() {
  var x = document.getElementById("bat");
    x.style.display = "block";

}
