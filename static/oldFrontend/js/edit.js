// JavaScript (c) by Bas Joosten 2009
// requires jQuer 1.3+

var editbox = document.createElement("input"); // an input box for user interaction
editbox.setAttribute("id","editbx");
editbox.setAttribute("name","editbx");
editbox.setAttribute("type","text");
var editObject=null; // current editing object. Null for none
var editContent=null; // the old value, used to restore the object, should we undo the edit
// editsNew tells whether the current editing object (ie editObject!=null) is new.
// This is used to answer these questions:
// should 'tab' give a new element?
// should unEdit(false) destroy the element?
var editsNew=false;
// tabHover is the current element we're hovering over, using the mouse or tab-key
var tabHover=null; // code to set it is written at the 'attatch hover/click events'


///////////////////////
//                   //
//   save function   //
//                   //
///////////////////////
function save(url,id){ // id can be used as an extra parameter, to set the ID value
  $('#errorPopup').remove(); // remove the previous error, if still present
  var all=($(".item").get()).concat($(".itemshow").get());
  var flds=new Array();
  for(var i=0;i<all.length;i++){
    var o=$(all[i]);
    if(o.is(':has(.item,.itemshow,.new)')){ // are there any nested attributes?
      var inp=$('input[name='+o.attr('id')+'.ID]');
      if(inp.is("*")) // does it exist?
        flds[i]=o.attr('id')+'='+escape(inp.val());
      else flds[i]=o.attr('id')+'='; // no id, but still set
    }else
    flds[i]=o.attr('id')+'='+escape($.trim(o.text())); //$.trim is needed to trim a trailing whitespace introduced by text() in LI items in IE
  }
  if(id!=null) flds[flds.length]='ID='+id;
  $.post(url,flds.join('&'),function recieveDataOnPost(data){
    var splitdata = data.split('ok:');
    if(splitdata.length==2){ // data is saved, follow URL (to exit edit mode)
      document.location.href=splitdata[1];
    } else {
      // create a error popup, and place it in the DOM immediately
      $('<DIV id=\"errorPopup\"><DIV id=\"errorClose\">x</DIV><DIV id=\"error\">'+data+'</DIV></DIV>').prependTo('.content:first').hide().slideDown();
      // add the functionality to the 'x' button
      $('#errorClose').click(function(){
        $('#errorPopup').slideUp(function(me){$('#errorPopup').remove();});
      });
    }
  });
}

// Set jQuery autocomplete values for edit field obj.
// The values are retrieved from the server with an atomList=<concept> request, so there is a slight
// delay before they are shown.
function setAutocomplete(obj) {
  var context = $(obj).attr('context');
  var interface = $(obj).attr('interface');
  var concept = $(obj).attr('concept');
  if (context && interface && concept) {
    $.post(context+".php",{ content: interface, atomList: concept },function receiveDataOnPost(data){
	  var resultOrError = JSON.parse(data); // contains .res or .err
	  if (typeof resultOrError.res !== 'undefined')
        $("input#editbx").autocomplete({ source:resultOrError.res }, { minLength: 0});
	  else
        console.error("Ampersand: Error while retrieving auto-complete values:\n"+resultOrError.err);
    });
  } else
    console.error("Ampersand: Missing 'context', 'interface' or 'concept' html attribute for auto-complete edit field");
}

//////////////////////////
//                      //
//   Editing function   //
//                      //
//////////////////////////
function edit(obj){
    if(obj==null) return;
    editsNew=false;
    if(tabHover!=null) $(tabHover).removeClass('hover');
    // get the UI class function
    cls = getUIClass(obj);
    if($(obj).is('.new')){
        ////////////////////
        //   create new   //
        ////////////////////
        if($(obj).is('div'))
          editObject=obj;
        else{
          editsNew=true;
          editObject=$(obj).clone().insertBefore(obj);
          var nrs=$(obj).attr('id').split('.');
          nrs[nrs.length-1]-=-1; // add one numerically
          $(obj).attr('id',nrs.join('.'));
        }
        if(eval('typeof '+cls)=='function'){
          ///////////////////////////////
          //   create complex object   //
          //  and edit its first item  //
          //                           //
          editsNew=false;
          $(editObject).html(eval(cls+'(\''+$(editObject).attr('id')+'\')'));
          reassign($(editObject).find(".new,.item"));
          reassign($(editObject));
          firstObj=$(editObject).find('.item:not(:has(.item,.new))').eq(0);
          edit(firstObj);
          $(editObject).removeClass("new");
          $(editObject).addClass("item");
          //                           //
          // unfortunately, the cancel //
          //   key won't work so well  //
          ///////////////////////////////
        }else{
          ///////////////////////////////
          // create a new leaf-element //
          //        and edit it        //
          reassign(editObject);
          editBoxIt(editObject).value='';
          $(editObject).removeClass("new");
          $(editObject).addClass("item");
          
          setAutocomplete(obj);
        }
    }else{
        //////////////////////
        //   edit current   //
        //////////////////////
        if(eval('typeof '+cls)=='function'){ // remove the item (since we can create a new one!)
          if(!$(obj).is('li')){ // not a list item: create my own 'New'
            var newitm=$('<DIV class="new '+cls+'" id="'+$(obj).attr('id')+'"><i>Nothing</i></DIV>');
            $(obj).replaceWith(newitm);
            reassign(newitm);
          }else{
            remove(obj);
          }
        } else {
          var txt=$.trim($(obj).text()); //$.trim is needed to trim a trailing whitespace introduced by text() in LI items in IE
          editBoxIt(obj).value=txt;

          setAutocomplete(obj);
        }
    }
}
////////////////////////////////////////
//                                    //
//..and the function to stop editing  //
//                                    //
////////////////////////////////////////
function unEdit(submit,reset){ if(reset==null) reset=true;
    if(editObject==null) return false;
    $(editObject).addClass('hover');
    tabHover=editObject;
    $(editObject).empty();
    if(!submit) editbox.value=editContent;
    if(editbox.value==''){
        if($(editObject).is('li')) {
          var obj=$(editObject);
          editObject=prev(obj,".item:not(:has(.item,.new))");
          // the line below ensures the next item won't be "new" again: move on to next field
          // comment the line to get blocking behaviour
          // editsNew=false;
          remove(obj);
        }else{
          if($(editObject).is('div')){
              var cls=getUIClass(editObject);
              var id=$(editObject).attr('id');
              var newitm=$('<DIV class="new '+cls+'" id="'+id+'"><i>Nothing</i></DIV>');
              $(editObject).replaceWith(newitm);
              reassign(newitm);
          } // in other cases, the object is a SPAN, which cannot be empty!
        }
    }else $(editObject).text(editbox.value);
    if(reset) editObject=null;
}

function remove(obj){
    var nrs = $(obj).attr('id').split('.');
    var i=nrs.pop()-0;
    var start=nrs.join('.');
    var jstart=nrs.join('\\.');
    if(nrs.length){ start+='.'; jstart+='\\.'; }
    for(i++;$('#'+jstart+i).attr('id')!=null;i++){
      $('#'+jstart+i).attr('id',start+(i-1));
    }
    $(obj).remove();
}

// change the item into an editbox, and ensure it is possible to change it back later
function editBoxIt(obj){
    editObject=obj;
    if($(obj).is('.item')) editContent=$(obj).get(0).innerHTML; else editContent='';
    $(obj).empty();
    $(obj).append(editbox);
    editbox.focus();
    $(tabHover).removeClass('hover');
    setTimeout('uneditBlur();',1);
    $(editbox).blur(function blurEditbox(){
      $(editbox).focus();
      return false;
    });
    return editbox;
}
function uneditBlur(){
    $(editbox).blur(function blurEditbox(){
      unEdit(true);
    });
}
function getUIClass(obj){
    var classes=$(obj).attr('class').split(' ');
    for(var i=0;i<classes.length;i++){
      if(classes[i].substr(0,2)=="UI") return classes[i];
    }
    return '';
}

//////////////////////////////////////
//                                  //
//   attatch hover / click events   //
//                                  //
//////////////////////////////////////
function reassign(objs){
   $(objs).hover(
     function hover_over(){
       if(editObject==null){
           if($(this).not(":has(.hover)").addClass('hover').is(".item,.new")){
              if(tabHover!==this) $(tabHover).removeClass('hover');
              tabHover=this;
           }
           $(this).parents().removeClass('hover');
       }
     }
     ,
     function hover_out(){
       if(editObject==null){
         $(this).removeClass('hover');
         if($(this).parents(".item").eq(0).addClass('hover').is(".item,.new")){
           if(tabHover!=$(this).parent().get(0)) $(tabHover).removeClass('hover');
           tabHover=$(this).parents(".item").eq(0);
         }
       }
     }
     );
   $(objs).click(
      function click(){ if($(this).is(".hover") && editObject==null) edit(this);
                      }
   );
}


/////////////////////////
//                     //
//   document onload   //
//                     //
/////////////////////////
$(function documentLoad() {
   reassign(".item,.new");
   $('form').submit(function formSubmitCatch(){
     return false;
   }
   );
   /////////////////////////
   //   keyboard events   //
   /////////////////////////
   document.onkeydown=function (e) {
      var kC;
      var shift;
      if ("which" in e)
      {// NN4 & FF &amp; Opera
        kC=e.which;
        shift=e.shiftKey;
      } else if ("keyCode" in e)
      {// Safari & IE4+
        kC=e.keyCode;
        shift=e.shiftKey;
      } else if ("keyCode" in window.event)
      {// IE4+
        kC=window.event.keyCode;
        shift=window.event.shiftKey;
      } else if ("which" in window.event)
      {
        kC=window.event.which;
        shift=window.event.shiftKey;
      }
      var Esc = (window.event) ? 27 : e.DOM_VK_ESCAPE;
      if(kC==13){//Br
        if(editObject==null && tabHover!=null && $(tabHover).is(".hover"))
        edit(tabHover);
        else if(editObject!=null) unEdit(true);
        else{
          // show a dialog to save the entire page ?
        }
      }else
      if(kC==Esc)
        {
          if(editObject==null){ $(tabHover).removeClass('hover'); tabHover=null}
          unEdit(false);
        }
      else
      if(kC==9){//Tab
        if(editObject!=null){
            unEdit(true,false);
            var search=".item:not(:has(.item,.new))";
            // search="(.item,.new):not(:has(.item,.new))" doesn't work due to a jQuer bug
            // however, .item:not(:has(.item,.new)),.new:not(:has(.item,.new)) will work
            // but we can use code below since .new never :has(.item,.new)
            if(editsNew){ search=".item:not(:has(.item,.new)),.new";}
            first=true;
            if(shift) editObject=prev($(editObject),search);
            else editObject=next($(editObject),search);
            edit(editObject.get(0));
        }else{
            tabHover=$(tabHover);
            tabHover.removeClass('hover');
            var search=".new,.item";
            if(shift) tabHover=prev(tabHover,search).get(0);
            else tabHover=next(tabHover,search).get(0);
            $(tabHover).addClass('hover');
        }
      }
      else {return true;}
      return false;
    };
});


///////////////////////////////
//                           //
//   some helper functions   //
//                           //
///////////////////////////////
function next(obj,search){
  if(obj.find(search).length){
    return obj.find(search).eq(0);
  }
  var nxt;
  while((nxt=obj.next()).length==0){
    if(obj.parent().length) obj=obj.parent();
    else return next($("body"),search);
  }
  if(nxt.is(search)) return nxt
  return next(nxt,search);
}
function prev(obj,search){
  while(obj.prev().length) {
    obj = obj.prev();
    var all=obj.find(search);
    if(all.length) return all.eq(all.length-1);
    if(obj.is(search)) return obj;
  }
  var all=obj.parent();
  if(all.is(search)) return all;
  if(all.length) return prev(all,search);
  var all=$("body").find(search)
  return all.eq(all.length-1);
}