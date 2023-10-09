$(document).ready(function(){
    $('input[name=ville_comp]').on('click', function(event){
      if($('input[name=ville_comp]:checked').length > 3){
        $(this).prop('checked', false);
      }
    });
    $('input[name=ville_comp]').on('click', function(event){
      if($('input[name=ville_comp]:checked').length == 0){
        $(this).prop('checked', true);
      }
    });
  });