function graf_hover(x, y) {
  graf_hover_info = document.getElementsByClassName('graf_hover_info').forEach(function(e)
  {
    console.log("graf_hover(x, y) = graf_hover("+x+","+y+")");
    e.style.left = (x+0)+"px";
    e.style.top = y+"px";  
  });
  
}


Shiny.addCustomMessageHandler(
  "hide",
  function(message) {
    console.log("hide "+message)
    setTimeout(function(){
      document.querySelectorAll(message).forEach(function(e) {
        e.style.display = "none";
      })
    }, 500);
  }
)

Shiny.addCustomMessageHandler(
  "show",
  function(message) {
    console.log("show "+message)
    setTimeout(function(){
      document.querySelectorAll(message).forEach(function(e) {
        e.style.display = "block";
      })
    }, 500);
  }
)

Shiny.addCustomMessageHandler(
  "steps",
  function(step) {
    console.log("steps "+step)
    setTimeout(function(){
      document.querySelectorAll(".steps").forEach(function(e) {
        e.className = "steps " + step;
      })
    }, 500);
  }
)


Shiny.addCustomMessageHandler(
  "reset_text",
  function(id) {
    console.log("id "+id)
    setTimeout(function(){
      $(id).val("")
    }, 500);
  }
)

