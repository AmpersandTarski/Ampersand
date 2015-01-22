// JavaScript (c) by Bas Joosten 2009
// requires jQuer 1.3+

/////////////////////////
//                     //
//   document onload   //
//                     //
/////////////////////////
$(function documentLoad() {
   $('.GotoArrow,.GotoLink').click(function(){
     var id='#Go'+$(this).attr('id').split('.').join('\\.');
     var offset=$(this).offset();
     $(id).css('top',offset.top-5);
     $(id).css('left',offset.left+$(this).width()+5);
     if($(id).css('display')=='none'){
       $('.Goto').hide();
       $(id).show();
     } else $(id).hide();
   });
});