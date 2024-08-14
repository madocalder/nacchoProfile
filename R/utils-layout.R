openMenu <- \(){
  tags$a(
    `data-bs-toggle` = "offcanvas",
    href = "#offcanvas-menu",
    role = "button",
    `aria-controls` = "offcanvas-menu",
    class = "offcanvas-menu-trigger text-decoration-none d-none",
    shiny::icon("bars", class = "fa-2x")
  )
}
