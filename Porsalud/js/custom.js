(function ($) {
    "use strict";

    /*Preloader*/
    $(window).preloader({
        delay: 500
    });

    // auto close navbar-collapse on click
    $('.nav-item>[data-scroll]').on('click', function () {
        $('.navbar-toggler:visible').click();
    });

    /*sticky navbar*/
    $("#navbar-sticky").sticky({ topSpacing: 0 });
    // transparent header fixed-top
    $(window).on("scroll",function () {
        var scroll = $(window).scrollTop();

        if (scroll >= 100) {
            $("#navbar-fixed-top").addClass("sticky-active");
        } else {
            $("#navbar-fixed-top").removeClass("sticky-active");
        }
    });

    /*smooth scroll*/
    smoothScroll.init({
        selector: '[data-scroll]', // Selector for links (must be a class, ID, data attribute, or element tag)
        speed: 800, // Integer. How fast to complete the scroll in milliseconds
        easing: 'easeInOutCubic', // Easing pattern to use
        offset: 70, // Integer. How far to offset the scrolling anchor location in pixels
        callback: function (anchor, toggle) { } // Function to run after scrolling
    });
})(jQuery);



