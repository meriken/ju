/* Localtime of User Agent.
 * Copyright (C) 2006-2014 shinGETsu Project.
 */

shingetsu.initialize(function () {
    function format(n) {
        return ('0' + n).substr(-2);
    }

    var tblE = new Array('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    var tblJ = new Array('\u65e5','\u6708','\u706b','\u6c34','\u6728','\u91d1','\u571f');
    var tbl = new Array(' ',' ',' ',' ',' ',' ',' ');
    if (location.pathname.match(/\/thread\.cgi\/.*/)) {
        for (var i=0; i<7; i++){
            if (shingetsu.uiLang == 'ja') {
                tbl[i] = '(' + tblJ[i] + ')';
            } else {
                tbl[i] = '(' + tblE[i] + ')';
            }
        }
    }

    function myLocaltime(date) {
        var year = date.getYear();
        if (year < 1900) year += 1900;
        var month = format(date.getMonth()+1);
        var day = format(date.getDate());
        var hours = format(date.getHours());
        var minutes = format(date.getMinutes());
        return year + '-' + month + '-' + day + ' ' + tbl[date.getDay()] + ' ' + hours + ':' + minutes;
    }

    function overrideDatetime($container) {
        $container.find('span.stamp[data-stamp]').each(function() {
            var container = $(this);
            var date = new Date();
            date.setTime(container.attr('data-stamp') * 1000);
            container.html(myLocaltime(date));
        });
    }

    shingetsu.addRecordsModifiers(overrideDatetime);
});
