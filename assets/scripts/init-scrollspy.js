{
  // Navbar table-of-contents
  const menu = document.querySelector('#nav-toc');
  const scrollspy = VanillaScrollspy({menu});
  scrollspy.init();
}

{
  // Sidebar table-of-contents
  const menu = document.querySelector('#side-toc');
  const scrollspy = VanillaScrollspy({menu});
  scrollspy.init();
}
