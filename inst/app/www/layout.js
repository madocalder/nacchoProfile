$(() => {
  insertOffcanvasMenu();

  layout();
  window.addEventListener("resize", layout);
});

const layout = () => {
  if (window.innerWidth > 991) {
    expand();
    return;
  }

  collapse();
};

const collapse = () => {
  const $sidebar = $("aside.sidebar").first().detach();
  const $oc = $("#offcanvas-menu .offcanvas-body");
  $oc.append($sidebar);

  $(".offcanvas-menu-trigger").removeClass("d-none");

  $(".bslib-sidebar-layout").find(".collapse-toggle").first().hide();
  $(".main").first().css("grid-column", "1 / 3");
};

const expand = () => {};

const offcanvasMenu = `
<div class="offcanvas offcanvas-start" tabindex="-1" id="offcanvas-menu" aria-labelledby="offcanvas-menu-label">
  <div class="offcanvas-body">
  </div>
</div>`;

const insertOffcanvasMenu = () => {
  $("body").append(offcanvasMenu);
};
