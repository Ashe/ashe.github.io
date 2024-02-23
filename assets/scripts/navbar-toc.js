const toc_trigger = document.getElementById('nav-toc-trigger');
const toc_menu = document.getElementById('nav-toc');

if (toc_trigger && toc_menu) {

  // Show hidden div on hover
  toc_trigger.addEventListener('mouseover', function handleMouseOver() {
    let isMobile = window.matchMedia("only screen and (max-width: 760px)").matches;
    if (!isMobile) {
      toc_menu.style.display = 'block';
    }
  });

  // Hide div on mouse out
  toc_menu.addEventListener('mouseleave', function handleMouseOut() {
    let isMobile = window.matchMedia("only screen and (max-width: 760px)").matches;
    if (!isMobile) {
      toc_menu.style.display = 'none';
    }
  });

  // Toggle visibility on click
  toc_trigger.addEventListener('click', function toggleVisibility() {
    if (toc_menu.style.display === "none") {
      toc_menu.style.display = "block";
    }
    else {
      toc_menu.style.display = "none";
    }
  });
}
