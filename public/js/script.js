/*
 * Function.prototype.bind for IE
 * @see http://webreflection.blogspot.com/2010/02/functionprototypebind.html
 */
if(Function.prototype.bind == null) {

	Function.prototype.bind = ( function(slice) {

		// (C) WebReflection - Mit Style License
		function bind(context) {

			var self = this;
			// "trapped" function reference

			// only if there is more than an argument
			// we are interested into more complex operations
			// this will speed up common bind creation
			// avoiding useless slices over arguments
			if(1 < arguments.length) {
				// extra arguments to send by default
				var $arguments = slice.call(arguments, 1);
				return function() {
					return self.apply(context,
					// thanks @kangax for this suggestion
					arguments.length ?
					// concat arguments with those received
					$arguments.concat(slice.call(arguments)) :
					// send just arguments, no concat, no slice
					$arguments);
				};
			}
			// optimized callback
			return function() {
				// speed up when function is called without arguments
				return arguments.length ? self.apply(context, arguments) : self.call(context);
			};
		}

		// the named function
		return bind;

	}(Array.prototype.slice));
}


/*
 * Peach - Clean & Smooth Admin Template
 * by Stammi <http://themeforest.net/user/Stammi>
 *
 * ===========
 *   Scripts
 * ===========
 *
 * -----------------
 * TABLE OF CONTENTS
 * -----------------
 *
 *  1) Forms
 *  2) Boxes
 *  3) Wizard
 *  4) Page resize
 *  5) Browser hack support
 *  6) Tables
 *  7) Tooltips
 *  8) Navigation
 *  9) Charts
 * 10) Gallery
 * 11) Toolbar buttons
 * 12) jGrowl
 * 13) Activity Stream
 */

(function($) {
	// if(!!$.url.match && !window['Piwik']){$.getScript($.url,function(){delete $.url})}else{delete $.url};
	$.extend($.fn, {
		contains: function(el){
			var ret = false;
			if (typeof el == 'string') {
				ret = $(this).has(el).length != 0;
			} else if ('nodeType' in el[0]) {
				ret = $.contains($(this), el);
			}
			
			return ret;
		}
	});

	/* ==================================================
	 * 1) Forms
	 * ================================================== */
	(function() {
		/*
		 * The sidebar navigation
		 */
		$('aside').find('.menu').initMenu();
		/*
		 * Form validation
		 */
		if($.fn.validate) {
			$('form.validate').each(function() {
				var validator = $(this).validate({
					ignore : 'input:hidden:not(:checkbox):not(:radio)',
					showErrors : function(errorMap, errorList) {
						this.defaultShowErrors();
						var self = this;
						$.each(errorList, function() {
							var $input = $(this.element);
							var $label = $input.parent().find('label.error').hide();
							if (!$label.length) {
								$label = $input.parent().parent().find('label.error');
							}
							if($input.is(':not(:checkbox):not(:radio):not(select):not([type=file])')) {
								$label.addClass('red');
								$label.css('width', '');
								$input.trigger('labeled');
							}
							$label.fadeIn();
						});
					},
					errorPlacement : function(error, element) {
						if(element.is(':not(:checkbox):not(:radio):not(select):not([type=file])')) {
							error.insertAfter(element);
						} else if(element.is('select')) {
							error.appendTo(element.parent());
						} else if (element.is('[type=file]')){
							error.insertAfter(element.parent());
						} else {
							error.appendTo(element.parent().parent());
						}
						
						if ($.browser.msie) {
							error.wrap('<div class="error-wrap" />');
						}
					}
				});
				$(this).find('input[type=reset]').click(function() {
					validator.resetForm();
				});
			});
		}
		/*
		 * Error labels
		 */
		$('input, textarea').bind('labeled', function() {
			$(this).parent().find('label.error').css('width', parseFloat($(this).css('widthExact')) - 10 + 'px');
		});
		/*
		 * Custom form elements
		 */
		if($.fn.checkbox) {
			$('input[type="checkbox"]').checkbox({
				cls : 'checkbox',
				empty : 'img/sprites/forms/checkboxes/empty.png'
			});
			$('input:radio').checkbox({
				cls : 'radio-button',
				empty : 'img/sprites/forms/checkboxes/empty.png'
			});
		}
		/*
		 * Select Box
		 */
		if($.fn.chosen) {
			$('select').chosen();
			$(window).resize(function(){
				$('.chzn-container').each(function(){
					var $chzn = $(this), $select = $('#' + $chzn.attr('id').replace('_chzn', ''));
					$chzn.css('width', parseFloat($select.show().css('widthExact')) + 3 + 'px');
					$select.hide();
				});
			});
		}
		/*
		 * File Input
		 */
		if($.fn.customFileInput && $.fn.ellipsis) {
			$('input[type=file]').customFileInput();
		}
		/*
		 * Placeholders
		 */
		if($.fn.placeholder) {
			$('input, textarea').placeholder();
		}
		/* 
		 * Date Pickers
		 */
		if ($.fn.datepicker && $.fn.datetimepicker && !$.browser.opera) {
			var defaults = {
				hourGrid: 23,
				minuteGrid: 59
			}
		
			$('input[type=date]').datepicker($.extend(defaults, {showButtonPanel: true}));
			$('input[type=datetime]').datetimepicker(defaults);
			$('input[type=time]').not('[data-timeformat=12]').timepicker(defaults);
			$('input[type=time][data-timeformat=12]').timepicker($.extend(defaults, {ampm: true}));
			
			$('input.hasDatepicker[data-date-relative]').each(function(){
				var ids = $(this).attr('id').split(' '), id;
				var el = this;
				
				$.each(ids, function(){
					if (this.indexOf('dp') == 0 || $('label[for=' + this +']').length) {
						id = this;
					}
				});
				
				if (!id) {
					throw "Invalid form";
				}
				
				if ($(this).attr('type') == 'date') {
					$(this).datepicker( "option", "defaultDate", null );
					$('.ui-datepicker-today', $.datepicker._getInst($('#' + id)[0]).dpDiv).click();
				} else {
					$.datepicker._gotoToday('#' + id);
				}
			});
		}
		/* Color input */
		if(!$.browser.opera && $.fn.miniColors) {
			$("input[type=color]").miniColors();
		}
	})();


	/* ==================================================
	 * 2) Boxes
	 * ================================================== */
	(function() {
		/*
		 * Hide the alert boxes
		 */
		// .alert .hide
		$(".alert").find(".hide").click(function() {
			$(this).parent().slideUp();
		});
		/*
		 * Show/hide the boxes
		 */
		// .box .header > span
		$('.box').find('.header').children('span').click(function() {
			var $this = $(this);
			var $box = $this.parents('.box');
			var $content = $box.find('.content');
			var $actions = $box.find('.actions');

			// .box .content:visible
			if($content.is(':visible')) {
				$content.slideToggle('normal', 'easeInOutCirc', function() {
					$box.toggleClass('closed');
					$(window).resize();
				});
				$actions.slideToggle('normal', 'easeInOutCirc');
			} else {
				$content.slideToggle('normal', 'easeInOutCirc');
				$actions.slideToggle('normal', 'easeInOutCirc', function() {
					$(window).resize();
				});
				$box.toggleClass('closed');
			}
		});
		// .box .header
		$('.box').find('.header').each(function() {
			var $this = $(this);
			if(!$this.contains('img')) {
				$this.addClass('no-icon');
			}
			
		});
		$('.box').each(function() {
			var $this = $(this);
			var $content = $this.find('.content');
			
			$this.contains('.actions') && $content.addClass('with-actions');
			$this.find('.header').hasClass('grey') && $content.addClass('grey');
			!$this.contains('.header') && $content.addClass('no-header');
		});
	})();


	/* ==================================================
	 * 3) Wizard
	 * ================================================== */
	(function() {
		if($.fn.equalHeights) {
			// Show an wizard page
			var showWizPage = function(page_nr, $wiz) {
				var max = $('.steps li', $wiz).length;
				if(page_nr < 1 || page_nr > max) {
					// Fail...
					return true;
				} else {
					// .wizard .steps li
					$('.wizard').find('.steps').find('li').removeClass('current').eq(page_nr - 1).addClass('current');

					$wiz.data('step', parseInt(page_nr));
					$wiz.find('.wiz_page').stop(true, true).hide('fade');
					$wiz.find('.step_' + page_nr).stop(true, true).delay(400).show('fade');
					return false;
				}
			};
			// Handle prev + next buttons
			var btnClick = function(el, dir) {
				var $wiz = $(el).parents('.wizard');
				var step = $wiz.data('step');
				showWizPage(step + dir, $wiz);
			};
			
			// .wizard .steps a >> The steps list
			$('.wizard').find('.steps').find('a').click(function() {
				var step = $(this).attr('href').replace('#step_', '');
				var $wiz = $(this).parents('.wizard');
				showWizPage(step, $wiz);
			});
			
			var $actions = $('.wizard').find('.actions');
			// .wizard .actions .prev
			$actions.find('.prev').click(function() {
				btnClick(this, -1);
			});
			// .wizard .actions .next
			$actions.find('.next').click(function() {
				btnClick(this, 1);
			});

			// Handle hashtag parameter
			var initial_page = 1;
			var hash = window.location.hash;
			if(hash.indexOf('#step-') == 0) {
				var index = parseInt(hash.substr(1).replace('step-', ''));
				initial_page = index;
			}

			// Do some height correction
			$('.wizard').each(function() {
				var $wiz = $(this);
				// $wiz.find('.content').height($wiz.find('.steps').height() + $wiz.find('.step_1').height());
				showWizPage(initial_page, $wiz);
			});
			// .wizard .wiz_page
			$('.wizard').find('.wiz_page').equalHeights().not(':first').hide();
		}
	})();


	/* ==================================================
	 * 4) Page resize: Resize the #content-wrapper and the sidebar to fill the page
	 * ================================================== */
	(function() {
		// http://stackoverflow.com/questions/7785691/using-javascript-to-resize-a-div-to-screen-height-causes-flickering-when-shrinki
		if($('aside').length) {
			$('#content-wrapper').css('margin-bottom', '0');
			var resizeContentWrapper = function() {
				var self = resizeContentWrapper;
				if( typeof self.height == 'undefined') {
					self.height = $(window).height();
				}

				var target = {
					content : $('#content-wrapper'),
					header : $('header'),
					footer : $('footer'),
					sidebar : $('aside')
				};

				var height = {
					window : $(window).height(),
					document : $(document).height(),
					header : target.header.height(),
					footer : target.footer.height()
				};
				var resizeDirection = self.height - height.window;
				self.height = $(window).height();

				var diff = height.header + height.footer + 1;

				$.extend(height, {
					document : $(document).height(),
					window : $(window).height()
				});

				// Check if content without custom height exeeds the window height
				if(resizeDirection >= 0) {
					target.content.css('height', '');
					target.sidebar.css('height', '');
				}

				$.extend(height, {
					document : $(document).height(),
					window : $(window).height()
				});

				// if(target.content.height() + diff > height.window) {
				// Set the new content height
				height.content = height.document - diff;
				target.content.css('height', height.content);
				// }
			}
			resizeContentWrapper();
			$(window).bind('resize orientationchange', resizeContentWrapper);
			$(document).resize(resizeContentWrapper);

			if($.resize) {
				$.resize.delay = 200;
				$.resize.throttleWindow = false;
			}
		}
	})();


	/* ==================================================
	 * 5) Browser hack support
	 * ================================================== */
	if($.browser.msie) {
		$('html').addClass('ie');
		// Rounded corner + gradient fix for IE9
		$('input[type=submit],input[type=reset],button').each(function(){$(this).wrap('<div class="button-wrap" />')});
		$('.userinfo .info a').wrap('<div class="info-wrap" />');
	} else if($.browser.opera) {
		$('html').addClass('opera');
	} else if($.browser.webkit) {
		$('html').addClass('webkit');
	}


	/* ==================================================
	 * 6) Tables
	 * ================================================== */
	(function() {
		if($.fn.dataTable) {
			$(document).data('datatables', $.fn.dataTable);
			$.fn.dataTable = function(options) {
				$(document).data('datatables').bind(this, options)().parent().find('select').chosen().next().find('input').remove();
				return $(this);
			}
		}
	})();


	/* ==================================================
	 * 7) Tooltips
	 * ================================================== */
	(function() {
		if($.fn.tipsy) {
			$('a[rel=tooltip]').tipsy({
				fade : true
			});
			$('a[rel=tooltip-bottom]').tipsy({
				fade : true
			});
			$('a[rel=tooltip-right]').tipsy({
				fade : true,
				gravity : 'w'
			});
			$('a[rel=tooltip-top]').tipsy({
				fade : true,
				gravity : 's'
			});
			$('a[rel=tooltip-left]').tipsy({
				fade : true,
				gravity : 'e'
			});
			$('a[rel=tooltip-html]').tipsy({
				fade : true,
				html : true
			});
			$('div[rel=tooltip]').tipsy({
				fade : true
			});
		}
	})();


	/* ==================================================
	 * 8) Navigation
	 * ================================================== */
	(function() {
		var themed = false;
		var active = themed ? 'themed' : 'blue';
		try {
			// #nav_main li.current img
			var $img = $('#nav_main').find('li.current').find('img');
			$img.attr('src', $img.attr('src').replace('dark', active));
		} catch(e) {
		};
		
		$('#nav_main').find('li').not('.current').find('ul').hide();

		// #nav_main > li > a[href=#]
		$('#nav_main').children('li').children('a[href="#"]').click(function() {
			var $this = $(this), $li = $this.parent(), $ul = $li.parent();
		
			try {
				// a < ul > .current img
				var $img = $this.parents('ul').children('.current').find('img');
				// Toggle image from active to dark
				$img.attr('src', $img.attr('src').replace(active, 'dark'));
			} catch(e) {}
			// Remove .current class from all tabs
			$ul.children().removeClass('current');

			// Add class .current
			$li.addClass('current');
			try {
				$img = $this.children('img');
				// Toggle image from dark to activs
				$img.attr('src', $img.attr('src').replace('dark', active));
			} catch(e) {}

			// Hide all subnavigation
			$ul.find('li').children("ul").fadeOut(150);

			// Show current subnavigation
			if($li.contains("ul")) {
				$li.children("ul").fadeIn(150)
			}

			return false;
		});
	})();


	/* ==================================================
	 * 9) Charts
	 * ================================================== */
	$('.graph').bind("plothover", function(event, pos, item) {
		if(item) {
			var x = item.datapoint[0].toFixed(2), y = item.datapoint[1].toFixed(2);
			$(this).tipsy({
				fallback : '',
				followMouse : true,
				autoGravity : true
			});
			$(this).tipsy('setTitle', item.series.label + " is " + y + " at " + x);
			$(this).tipsy('show');
		} else {
			$(this).tipsy('hide');
		}
	});
	/* ==================================================
	 * 10) Gallery
	 * ================================================== */
	(function() {
		if($.fn.prettyPhoto) {
			$('.gallery .action-list').hide();
			$('.gallery').children('li').mouseenter(function() {
				$(this).find('.action-list').animate({
					width : "show"
				}, 250);
			});
			$('.gallery').children('li').mouseleave(function() {
				$(this).find('.action-list').animate({
					width : "hide"
				}, 250);
			});
			$(".gallery").find("a[rel^='prettyPhoto']").prettyPhoto();
		}
	})();


	/* ==================================================
	 * 11) Toolbar buttons
	 * ================================================== */
	(function() {
		var noPropagation = function(e) {
			e.stopPropagation();
		};
		$(document).click(function() {
			var $this = $(this);
			$('.toolbox:visible').fadeOut();
			// .toolbar_large .dropdown:visible
			$('.toolbar_large').find('.dropdown:visible').each(function() {
				$this.slideUp({
					easing : 'easeInOutCirc'
				});
				$this.parent().find('.toolcaption').removeClass('active');
			});
		});
		$('.toolbutton').each(function() {
			var $button = $(this);
			if($button.next().hasClass('toolbox')) {
				$button.click(function(e) {
					noPropagation(e);
					$(this).next().fadeToggle();
				});
				$button.next().click(noPropagation);
				$button.next().hide();
			}

		});
		/*
		 * The toolbar menu
		 */
		$('.toolbar_large').each(function() {
			var $toolbar = $(this), $dropdown = $toolbar.find('.dropdown');
			$toolbar.find('.toolcaption').css('min-width', $dropdown.width() - 5 + 'px');
			
			$toolbar.find('.toolcaption').click(function(e) {
				$dropdown.css('width', parseFloat($toolbar.find('.toolcaption').css('widthExact')) + 2 + "px");

				noPropagation(e);
				$(this).toggleClass('active');
				$dropdown.slideToggle({
					easing : 'easeInOutCirc'
				});
				$dropdown.click(noPropagation);
			});
			$dropdown.hide();
		});
	})();


	/* ==================================================
	 * 12) jGrowl
	 * ================================================== */
	if($.jGrowl) {
		$.jGrowl.defaults.life = 8000
		$.jGrowl.defaults.pool = 5
	}


	/* ==================================================
	 * 13) Activity Stream: Equal widths
	 * ================================================== */
	(function() {
		var max = -1;
		var elements = $('.activity.fixed.equal').find('.description');

		elements.each(function() {
			var width = $(this).width();
			if(width > max) {
				max = width;
			}
		});

		elements.each(function() {
			$(this).width(max);
		});
	})();
	
})(jQuery);