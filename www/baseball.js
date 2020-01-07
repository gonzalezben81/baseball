//Document Key Down--Enter Key Response to Search Button
//    $(document).on("keydown", "#search", function(e) {
  //    if (e.which == 13) {
    //    $("#search_button").click();
    //  }
    //});


$(document).on('shiny:connected', function(event) {
  alert('Welcome to the Major League Baseball Statistics App');
});

$(document).on('shiny:connected', function(event) {
  if (event.name === 'search') {
    event.value = "Babe Ruth";
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
  document.getElementById("bat").style.display = "block";
  document.getElementById("batters").style.display = "block";
//  var x = document.getElementById("bat");
  //  x.style.display = "block";
//var y = document.getElementById("batters");
  //  y.style.display = "block";

}

function autocomplete() {
    var availableTags = [
      "ActionScript",
      "AppleScript",
      "Asp",
      "BASIC",
      "C",
      "C++",
      "Clojure",
      "COBOL",
      "ColdFusion",
      "Erlang",
      "Fortran",
      "Groovy",
      "Haskell",
      "Java",
      "JavaScript",
      "Lisp",
      "Perl",
      "PHP",
      "Python",
      "Ruby",
      "Scala",
      "Scheme"
    ];
    $( "#search" ).autocomplete({
      source: availableTags
    });
  }


function myFunctionalert() {
  alert("I am an alert box!");
}