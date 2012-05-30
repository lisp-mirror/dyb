// usage: log('inside coolFunc', this, arguments);
// paulirish.com/2009/log-a-lightweight-wrapper-for-consolelog/
window.log = function() {
	log.history = log.history || [];
	// store logs to an array for reference
	log.history.push(arguments);
	if(this.console) {
		arguments.callee = arguments.callee.caller;
		var newarr = [].slice.call(arguments); ( typeof console.log === 'object' ? log.apply.call(console.log, console, newarr) : console.log.apply(console, newarr));
	}
};
// make it safe to use console.log always
(function(b) {
	function c() {
	}

	for(var d = "assert,clear,count,debug,dir,dirxml,error,exception,firebug,group,groupCollapsed,groupEnd,info,log,memoryProfile,memoryProfileEnd,profile,profileEnd,table,time,timeEnd,timeStamp,trace,warn".split(","), a; a = d.pop(); ) {
		b[a] = b[a] || c
	}
})((function() {
	try {console.log();
		return window.console;
	} catch(err) {
		return window.console = {};
	}
})());

/*
 * Peach - Clean & Smooth Admin Template
 * by Stammi <http://themeforest.net/user/Stammi>
 *
 * ===========
 *   Plugins
 * ===========
 *
 * -----------------
 * TABLE OF CONTENTS
 * -----------------
 *
 * 1) Menu
 * 2) Alert Boxes
 *    a) Create
 *	  b) Remove
 * 3) Tabs
 * 4) CSS height/width hook
 */

/* ==================================================
 * 1) Menu by Simon Stamm
 * ================================================== */
jQuery.fn.initMenu = function() {
	return $(this).each(function() {
		var $menu = $(this);
		
		// Set the container's height
		$menu.find('.sub').show();
		$menu.parent().height($menu.height() + 10);
		$menu.find('.sub').hide();

		// Append arrow to submenu items
		$menu.find('li:has(ul)').each(function() {
			$(this).children('a').append("<span class='arrow'>&raquo;</span>");
		});
		$menu.find('.sub').hide();
		
		// The main part
		$menu.find('li a').click(function(e) {
			e.stopImmediatePropagation();
			var $submenu = $(this).next(), $this = $(this);
			
			if($menu.hasClass('noaccordion')) {
				if($submenu.length == 0) {
					window.location.href = this.href;
				}
				$submenu.slideToggle('normal');
				return false;
			} else {
				// Using accordeon
				if($submenu.hasClass('sub') && $submenu.is(':visible')) {
					// If already visible, slide up
					if($menu.hasClass('collapsible')) {
						$menu.find('.sub:visible').slideUp('normal');
						return false;
					}
					return false;
				} else if($submenu.hasClass('sub') && !$submenu.is(':visible')) {
					// If not visible, slide down
					$menu.find('.sub:visible').slideUp('normal');
					$submenu.slideDown('normal');
					return false;
				}
			}
		});
	});
}; 
/* ==================================================
 * 2) Alert Boxes by Simon Stamm
 * ================================================== */

/* ==================================================
 * 2a) Alert Boxes: Create
 * ================================================== */
(function($) {
	$.fn.alertBox = function(message, options) {
		var settings = $.extend({}, $.fn.alertBox.defaults, options);

		this.each(function(i) {
			var block = $(this);

			var alertClass = 'alert ' + settings.type;
			if(settings.noMargin) {
				alertClass += ' no-margin';
			}
			if(settings.position) {
				alertClass += ' ' + settings.position;
			}
			var alertMessage = $('<div style="display:none" class="' + alertClass + ' .generated">' + message + '</div>');
			if (settings.icon) {
				alertMessage.prepend($('<span>').addClass('icon'));
			}

			var alertElement = block.prepend(alertMessage);

			$(alertMessage).fadeIn();
		});
	};
	// Default config for the alertBox function
	$.fn.alertBox.defaults = {
		type : 'info',
		position : 'top',
		noMargin : true,
		icon: false
	};
})(jQuery);

/* ==================================================
 * 2b) Alert Boxes: Remove
 * ================================================== */

(function($) {
	$.fn.removeAlertBoxes = function() {
		var block = $(this);

		var alertMessages = block.find('.alert');
		alertMessages.fadeOut(function(){$(this).remove()});
	};
})(jQuery);

/* ==================================================
 * 3) Tabs by Simon Stamm
 * ================================================== */

(function($){
	$.fn.createTabs = function(){
		var container = $(this), tab_nr = 0;
		
		container.find('.tab-content').hide();
		
		// Open tab by hashtag
		if (window.location.hash.indexOf('#tab') == 0) {
			var hash = window.location.hash.substr(1);
			console.log(hash);
			if (typeof hash == 'number') {
				var tmp = parseInt(window.location.hash.substr(1), 10);
				if (tmp > 0 && tmp < container.find('.tab-content').size()) {
					tab_nr = tmp - 1;
				}			
			} else {
				var tab_name = container.find('#' + hash.replace('tab-', '') + '.tab-content');
				if (tab_name.size() && tab_name.not(':visible')) {
					tab_nr = tab_name.index();
				}
			}
		}
		
		container.find(".header").find("li").eq(tab_nr).addClass("current").show();
		container.find(".tab-content").eq(tab_nr).show();
		
		container.find(".header").find("li").click(function() {
			container.find(".header").find("li").removeClass("current");
			$(this).addClass("current");
			container.find(".tab-content").hide();
	
			var activeTab = $(this).find("a").attr("href");
			$(activeTab).fadeIn();
			return false;
		});
		
	};
})(jQuery);

/* ==================================================
 * 4) CSS height/width hook
 * ================================================== */

/*
 * Because jQuery rounds the values :-/
 */
(function($) {
	if ($.browser.msie) {
		if (!window.getComputedStyle) {
			window.getComputedStyle = function(el, pseudo) {
				this.el = el;
				this.getPropertyValue = function(prop) {
					var re = /(\-([a-z]){1})/g;
					if (prop == 'float') prop = 'styleFloat';
					if (re.test(prop)) {
						prop = prop.replace(re, function () {
							return arguments[2].toUpperCase();
						});
					}
					return el.currentStyle[prop] ? el.currentStyle[prop] : null;
				}
				return this;
			}
		}
	}

	var dir = ['height', 'width'];
	$.each(dir, function() {
		var self = this;
		$.cssHooks[this + 'Exact'] = {
			get : function(elem, computed, extra) {
				return window.getComputedStyle(elem)[self];
			}
		};
	});
})(jQuery);