jQuery(function($) {

  // ----------------------------------------------------------------- //

  jQuery( 'body' ).on( 'click', '.register-action, .register-here', function(e) {

    e.preventDefault();

    $( '#login' ).hide();

    if( document.getElementById( 'register' ) !== null ) {
      $( '#register' ).show();
    } else {
      $.ajax({
        type : 'POST',
        url  : ajax.ajax_url,
        data : {
          action : 'registerForm'
        },
        success : function( response ) {
          $( 'body' ).append( response );
        }
      });
    }

  });

  // ----------------------------------------------------------------- //
  
  jQuery( '.login-action' ).on( 'click', function(e) {

    e.preventDefault();

    if( document.getElementById( 'login' ) !== null ) {
      $( '#login' ).show();
    } else {

      $.ajax({
        type : 'POST',
        url  : ajax.ajax_url,
        data : {
          action : 'loginForm'
        },
        success : function( response ) {
          $( 'body' ).append( response );
        }
      });

    }

  });

  // ----------------------------------------------------------------- //

  jQuery( 'body' ).on( 'click', '.close-popup', function() {

    $( this ).parents( '#login, #register, #whatsapp' ).hide();
    $( this ).parents( '#query' ).remove();

  });

  // ----------------------------------------------------------------- //

  jQuery( 'body' ).on( 'click', '#register button', function() {

    $.ajax({
      type : 'POST',
      url  : ajax.ajax_url,
      data : {
        action   : 'sendUserRegistration',
        username : $( '#register' ).find( 'input[name=username]' ).val(),
        email    : $( '#register' ).find( 'input[name=email]' ).val(),
        password : $( '#register' ).find( 'input[name=password]' ).val()
      },
      success : function( response ) {
        window.location.href = response.data.location;
      }
    });

  });

  // ----------------------------------------------------------------- //

  jQuery( 'body' ).on( 'click', '#login button', function() {

    $.ajax({
      type : 'POST',
      url  : ajax.ajax_url,
      data : {
        action   : 'sendUserLogin',
        username : $( '#login' ).find( 'input[name=username]' ).val(),
        password : $( '#login' ).find( 'input[name=password]' ).val()
      },
      success : function( response ) {
        window.location.href = response.data.location;
      }
    });

  });

  // ----------------------------------------------------------------- //
  // ----------------------------------------------------------------- //
  // ----------------------------------------------------------------- //
  
  jQuery( '.search-holder' ).on( 'click', function() {

    $( '.search-container' ).show();

  });

  // ----------------------------------------------------------------- //

  jQuery( 'body' ).on( 'click', '.close-search', function() {
    $( '.search-container' ).removeAttr( 'style' );
  });

  // ----------------------------------------------------------------- //
  // ----------------------------------------------------------------- //
  // ----------------------------------------------------------------- //

  function responsive_video() {
      $('.responsive-video, .wp-video').each(function(){
        var fluid = $(this),
            video = fluid.children(),
            ratio = video.attr('height') / video.attr('width'),
            fluidWidth = fluid.width();
      
        video
            .removeAttr('height').removeAttr('width')
            .width( fluidWidth ).height( fluidWidth * ratio );
      
        $(window).resize(function(){
            var newFluidWidth = fluid.width();
            video
              .width( newFluidWidth )
              .height( newFluidWidth * ratio );
        });
    });
  }

  responsive_video();

// ----------------------------------------------------------------- //

  function gallery() {
      var gallery = $('.mod--gallery');

      if( gallery.length ) {
        var next    = gallery.find('.next'),
            prev    = gallery.find('.prev'),
            
            counter = 0,
            items   = gallery.find('li'),
            amount  = items.length,
            current = $( items[0] ),
            dir     = 0;

        next.on('click', function(){
          current.removeClass('current');
          
          counter = counter + 1;
          
          if( !items[counter] ) {
            counter = 0;
          }
          
          current = $( items[counter] );  
          current.addClass('current');
        });
        
        prev.on('click', function(){
          current.removeClass('current');
          
          counter = counter - 1;
          
          if( counter < 0 ) {
            counter = amount - 1;
          }
          
          current = $( items[counter] );  
          current.addClass('current');
        });
      }
  }

  gallery();

// ----------------------------------------------------------------- //

  function doMediaQueries() {
      if (window.matchMedia('(max-width: 640px)').matches) {

        var searchBox = $( '.search-container' ).clone();
        $( '.nav-sections ul' ).prepend( searchBox );
          
          	// Evento para el menú
          	$( '.nav-sections' ).on( 'click', function(e) {
            	if (e.target !== this)
            	return;
          		// Cambia el estado del menú
            	$( '.nav-sections ul' ).toggle();
            	// Agrega una class para evitar el scroll de body
            	$( 'body' ).toggleClass( 'no-scroll' );
          	});

          	// Evento para el menú usuario
          	$( '.usermenu' ).on( 'click', function(e) {
          		if (e.target !== this)
            	return;

            	$( this ).toggleClass( 'active' );
            	// Cambia el estado del menú
            	$( '.usercontainer' ).toggleClass( 'show-container' );
            	// Agrega una class para evitar el scroll de body
            	$( 'body' ).toggleClass( 'no-scroll' );

          	});

      } else {
        $( '.nav-sections ul .search-container').remove();
      }
  }

  doMediaQueries();

// ----------------------------------------------------------------- //

  jQuery( 'body' ).on( 'click', '.quiz ul li.quiz-option', function() {

    var option     = $( this ).find( 'input[name=quiz-choice-id]' ),
      optionId   = option.val(),
      quizId     = $( this ).parents( '.quiz' ).find( 'input[name=quiz-id]' ).val(),
      parentQuiz = $( this ).parents( '.quiz' );

      if( !option.is( ':disabled' ) ) {

        parentQuiz.find( 'input[type=radio]' ).attr( 'disabled', true );
        $( '.quiz ul li.quiz-option' ).addClass( 'fade' );
        $( this ).removeClass( 'fade' ).addClass( 'selected' );

        $.ajax({
          type : 'POST',
          url  : ajax.ajax_url,
          data : {
            action   : 'voteSurvey',
            optionId : optionId,
            quizId   : quizId
          },
          success : function( response ) {
            response = jQuery.parseJSON( response );

            $( parentQuiz ).append( response.results );
          }
        });

      }

  });

	// ----------------------------------------------------------------- //

	$('.scroll-right').click(function() {
	  var far = $( 'div.matches-container' ).width() / 3 + 98;
	  var pos = $('div.matches-container').scrollLeft() + far;
	  $('div.matches-container').animate( { scrollLeft: pos }, 1000 );
	});

	$('.scroll-left').click(function() {
	  var far = $( 'div.matches-container' ).width() / 3 + 98;
	  var pos = $('div.matches-container').scrollLeft() - far;
	  $('div.matches-container').animate( { scrollLeft: pos }, 1000 );
	});

  // ----------------------------------------------------------------- //

  window.fbAsyncInit = function() {
      FB.init({
          appId            : '223200044394668',
          //appId            : '1850893798466468',
          status           : true,
          cookie           : true,
          version          : 'v2.12'                
      });
      
      $( '.horoscope-button' ).click(function(e){
          e.preventDefault();

          var dataUrl       = $( this ).data( 'url' ),
            dataHoroscope = $( this ).data( 'horoscope' ),
            dataSign      = $( this ).data( 'sign' );
      
      // https://stackoverflow.com/questions/23514157/using-share-open-graph-facebook-ui-to-create-dynamic-share-dialog-for-quiz-res
          FB.ui(
                  {
                      method: 'share_open_graph',
              action_type: 'og.shares',
              action_properties: JSON.stringify({
                  object : {
                     'og:url': 'http://www.lr21.com.uy/horoscopo', // your url to share
                     'og:title': dataSign,
                     'og:description': dataHoroscope,
                     'og:image': 'https://ak9.picdn.net/shutterstock/videos/18681329/thumb/1.jpg'
                  }
              })
                  },
                  function (response) {
   
                  }
              );
      })
  };
   
  (function(d, s, id){
      var js, fjs = d.getElementsByTagName(s)[0];
      if (d.getElementById(id)) {return;}
      js = d.createElement(s); js.id = id;
      js.src = "//connect.facebook.net/es_LA/sdk.js";
      fjs.parentNode.insertBefore(js, fjs);
  }(document, 'script', 'facebook-jssdk'));

});