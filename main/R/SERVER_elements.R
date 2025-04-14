
TABLE_show <- function(table_nam, session) {
  DT::renderDataTable({
    get_data <- reactivePoll(5000, 
      session   =session,
      checkFunc = function() {
        dbtable_is_updated(table_nam)
      },
      valueFunc = function() {
        DBq(glue("select * FROM {table_nam}"))[, ":="(pk = NULL, nov = NULL)] 
      }
    )
    get_data()
  },
  server        = FALSE,
  rownames      = FALSE,
  escape        = FALSE,
  extensions    = c("Scroller", "Buttons"),
  options       = list(
    dom         = "Blfrtip",
    buttons     = list("copy", list(
      extend = "collection",
      buttons = "excel",
      text = "Download"
    )),
    scrollX     = "600px",
    deferRender = TRUE,
    scrollY     = 900,
    scroller    = TRUE,
    searching   = TRUE,
    columnDefs  = list(list(className = "dt-center", targets = "_all"))
  ),
  class = c("compact", "stripe", "order-column", "hover")
  )
}
  

ErrToast <- function(msg){
  bs4Dash::toast(
    
    title = "Moin!",
    
    body = msg |> a(class = "text-primary font-weight-bold") |> h4(),
        
    options = list(
      autohide = FALSE,
      close    = TRUE,
      position = "topLeft",
      icon     = "fa-solid fa-face-sad-tear"
    )
  
  )


}

WarnToast <- function(msg){
  bs4Dash::toast(
    
    title = "Moin!",
    
    body = msg |> a(class = "text-primary font-weight-bold") |> h4(),
        
    options = list(
      delay    = 10000,
      autohide = TRUE,
      close    = TRUE,
      position = "bottomRight",
      icon     = "fa-solid fa-face-sad-tear"
    )
  
  )


}

# the last element of ...  can have length > 1
startApp <- function(..., labels, host, isShiny = TRUE, class = "primary") {

  ddd = list(...)
  
  midpath = ddd[-(length(ddd))] |> unlist() |> paste(collapse = "/")
  basepath = ddd[[length(ddd)]]
  
  if(isShiny)
    hrefs <- glue("http://{host}:3838/{midpath}/{basepath}")
  if(!isShiny)
    hrefs <- glue("http://{host}/{midpath}/{basepath}")

  o = glue('
      <a  href="{hrefs}" target = "blank" class="btn btn-sm btn-{class} bttn bttn-fill bttn-md bttn-primary bttn-no-outline" role="button" >
      <h4> {labels} </h4>
    </a>
    ') |>
    glue_collapse()

  div(HTML(o), class = "d-grid gap-3 mx-auto mr-3")

  
}
