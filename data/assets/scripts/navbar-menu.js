const trigger = document.getElementById('navbar-expand-trigger');
const menu = document.getElementById('expanded-navbar');

// Show hidden div on hover
trigger.addEventListener('mouseover', function handleMouseOver() {
  menu.style.display = 'block';
});

// Hide div on mouse out
menu.addEventListener('mouseleave', function handleMouseOut() {
  menu.style.display = 'none';
});

// Toggle visibility on click
trigger.addEventListener('click', function toggleVisibility() {
  if (menu.style.display === "none") {
    menu.style.display = "block";
  }
  else {
    menu.style.display = "none";
  }
});
