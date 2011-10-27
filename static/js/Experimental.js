function initializeLinks() {
  $(".Atom").click(function () {
    console.log("click "+$(this).attr('class'));
    atom = $(this).attr('atom');
    console.log(" "+atom);
    window.location.href = "Interfaces.php?interface=Id&atom="+atom;
  });

}