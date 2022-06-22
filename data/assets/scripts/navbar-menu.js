const nav_trigger = document.getElementById('nav-menu-trigger');
const nav_menu = document.getElementById('nav-menu');

if (nav_trigger && nav_menu) {

  // Show hidden div on hover
  nav_trigger.addEventListener('mouseover', function handleMouseOver() {
    nav_menu.style.display = 'block';
  });

  // Hide div on mouse out
  nav_menu.addEventListener('mouseleave', function handleMouseOut() {
    nav_menu.style.display = 'none';
  });

  // Toggle visibility on click
  nav_trigger.addEventListener('click', function toggleVisibility() {
    if (nav_menu.style.display === "none") {
      nav_menu.style.display = "block";
    }
    else {
      nav_menu.style.display = "none";
    }
  });
}
