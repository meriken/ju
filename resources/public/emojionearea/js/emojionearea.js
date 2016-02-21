(function (document, window, $) {
    'use strict';

    var blankImg = 'data:image/gif;base64,R0lGODlhAQABAJH/AP///wAAAMDAwAAAACH5BAEAAAIALAAAAAABAAEAAAICVAEAOw==';

    var default_options = {
        template          : "<editor/><filters/><tabs/>",

        dir               : "ltr",
        spellcheck        : false,
        autocomplete      : "off",
        autocorrect       : "off",
        autocapitalize    : "off",

        placeholder       : null,
        container         : null,
        hideSource        : true,
        autoHideFilters   : false,

        shortnames        : false,
        useSprite         : true,

        filters: {
            people: {
                icon: "relaxed",
                emoji: "grinning,grin,joy,smiley,smile,sweat_smile,laughing,innocent,smiling_imp,imp,wink,blush," +
                "relaxed,yum,relieved,heart_eyes,sunglasses,smirk,neutral_face,expressionless,unamused,sweat," +
                "pensive,confused,confounded,kissing,kissing_heart,kissing_smiling_eyes,kissing_closed_eyes," +
                "stuck_out_tongue,stuck_out_tongue_winking_eye,stuck_out_tongue_closed_eyes,disappointed,worried," +
                "angry,rage,cry,persevere,triumph,disappointed_relieved,frowning,anguished,fearful,weary," +
                "sleepy,tired_face,grimacing,sob,open_mouth,hushed,cold_sweat,scream,astonished,flushed," +
                "sleeping,dizzy_face,no_mouth,mask,slight_frown,slight_smile," +

                "footprints," +
                "bust_in_silhouette,busts_in_silhouette,levitate,spy,baby,boy,girl,man,woman,family," +
                //"family_mwg,family_mwgb,family_mwbb,family_mwgg,family_wwb,family_wwg,family_wwgb,family_wwbb," +
                //"family_wwgg,family_mmb,family_mmg,family_mmgb,family_mmbb,family_mmgg," +
                "couple,two_men_holding_hands," +
                "two_women_holding_hands,dancers,bride_with_veil,person_with_blond_hair,man_with_gua_pi_mao," +
                "man_with_turban,older_man,older_woman,cop,construction_worker,princess,guardsman,angel," +
                "santa,ghost,japanese_ogre,japanese_goblin,poop,skull,alien,space_invader,bow," +
                "information_desk_person,no_good,ok_woman,raising_hand,person_with_pouting_face,person_frowning," +
                "massage,haircut,couple_with_heart," +
                //"couple_ww,couple_mm," +
                "couplekiss," +
                //"kiss_ww,kiss_mm," +
                "raised_hands," +
                "clap,ear,eye,eyes,nose,lips,kiss,tongue,nail_care,wave,thumbsup,thumbsdown,point_up,"+
                "point_up_2,point_down,point_left,point_right,ok_hand,v,punch,fist,raised_hand," +
                //"muscle,open_hands,writing_hand,hand_splayed,middle_finger,vulcan,pray"
                "muscle,open_hands,hand_splayed,middle_finger,vulcan,pray"
           },

            nature: {
                icon: "penguin",
                emoji: "seedling,evergreen_tree,deciduous_tree,palm_tree,cactus,tulip,cherry_blossom,rose,hibiscus," +
                "sunflower,blossom,bouquet,ear_of_rice,herb,four_leaf_clover,maple_leaf,fallen_leaf,leaves," +
                "mushroom,chestnut,rat,mouse2,mouse,hamster,ox,water_buffalo,cow2,cow,tiger2,leopard," +
                "tiger,chipmunk,rabbit2,rabbit,cat2,cat," +

                "smile_cat,joy_cat,smiley_cat," +
                "heart_eyes_cat,smirk_cat,kissing_cat,pouting_cat,crying_cat_face,scream_cat," +

                "racehorse,horse,ram,sheep,goat,rooster,chicken," +
                "baby_chick,hatching_chick,hatched_chick,bird,penguin,elephant,dromedary_camel,camel,boar,pig2," +
                "pig,pig_nose,dog2,poodle,dog,wolf,bear,koala,panda_face,monkey_face,see_no_evil,hear_no_evil," +
                "speak_no_evil,monkey,dragon,dragon_face,crocodile,snake,turtle,frog,whale2,whale,dolphin," +
                "octopus,fish,tropical_fish,blowfish,shell,snail,bug,ant,bee,beetle,spider,spider_web,feet," +
                "zap,fire,crescent_moon,sunny,partly_sunny,cloud,cloud_rain,cloud_snow,cloud_lightning,cloud_tornado," +
                "droplet,sweat_drops,umbrella,fog,dash,snowflake,star2,star,stars,sunrise_over_mountains,sunrise," +
                "rainbow,ocean,volcano,milky_way,mount_fuji,japan,globe_with_meridians,earth_africa,earth_americas," +
                "earth_asia,new_moon,waxing_crescent_moon,first_quarter_moon,waxing_gibbous_moon,full_moon," +
                "waning_gibbous_moon,last_quarter_moon,waning_crescent_moon,new_moon_with_face,full_moon_with_face," +
                "first_quarter_moon_with_face,last_quarter_moon_with_face,sun_with_face,wind_blowing_face"
            },

            food_drink: {
                icon: "tangerine",
                emoji: "tomato,eggplant,corn,sweet_potato,hot_pepper,grapes,melon,watermelon,tangerine,lemon," +
                "banana,pineapple,apple,green_apple,pear,peach,cherries,strawberry,hamburger,pizza,meat_on_bone," +
                "poultry_leg,rice_cracker,rice_ball,rice,curry,ramen,spaghetti,bread,fries,dango,oden,sushi," +
                "fried_shrimp,fish_cake,icecream,shaved_ice,ice_cream,doughnut,cookie,chocolate_bar,candy," +
                "lollipop,custard,honey_pot,cake,bento,stew,egg,fork_and_knife,tea,coffee,sake,wine_glass," +
                "cocktail,tropical_drink,beer,beers,baby_bottle"
            },

            celebration: {
                icon: "flags",
                emoji: "ribbon,gift,birthday,jack_o_lantern,christmas_tree,tanabata_tree,bamboo,rice_scene," +
                "fireworks,sparkler,tada,confetti_ball,balloon,dizzy,sparkles,boom,mortar_board,crown," +
                "reminder_ribbon,military_medal,dolls,flags,wind_chime,crossed_flags,izakaya_lantern,ring," +
                "heart,broken_heart,love_letter,two_hearts,revolving_hearts,heartbeat,heartpulse,sparkling_heart," +
                "cupid,gift_heart,heart_decoration,purple_heart,yellow_heart,green_heart,blue_heart"
            },

            activity: {
                icon: "soccer",
                emoji: "runner,walking,dancer,lifter,golfer,rowboat,swimmer,surfer,bath,snowboarder,ski," +
                "snowman,bicyclist,mountain_bicyclist,motorcycle,race_car,horse_racing,tent,fishing_pole_and_fish," +
                "soccer,basketball,football,baseball,tennis,rugby_football,golf,trophy,medal,running_shirt_with_sash," +
                "checkered_flag,musical_keyboard,guitar,violin,saxophone,trumpet,musical_note,notes,musical_score," +
                "headphones,microphone,performing_arts,ticket,tophat,circus_tent,clapper,film_frames,tickets," +
                "art,dart,8ball,bowling,slot_machine,game_die,video_game,flower_playing_cards,black_joker," +
                "mahjong,carousel_horse,ferris_wheel,roller_coaster"
            },

            travel: {
                icon: "bullettrain_front",
                emoji: "railway_car,mountain_railway,steam_locomotive,train,monorail,bullettrain_side," +
                "bullettrain_front,train2,metro,light_rail,station,tram,railway_track,bus,oncoming_bus," +
                "trolleybus,minibus,ambulance,fire_engine,police_car,oncoming_police_car,rotating_light,taxi," +
                "oncoming_taxi,red_car,oncoming_automobile,blue_car,truck,articulated_lorry,tractor,bike," +
                "motorway,busstop,fuelpump,construction,vertical_traffic_light,traffic_light,rocket,helicopter," +
                "airplane,airplane_small,airplane_departure,airplane_arriving,seat,anchor,ship,cruise_ship," +
                "motorboat,speedboat,sailboat,aerial_tramway,mountain_cableway,suspension_railway," +
                "passport_control,customs,baggage_claim,left_luggage,yen,euro,pound,dollar,bellhop,bed," +
                "couch,fork_knife_plate,shopping_bags,statue_of_liberty,moyai,foggy,tokyo_tower,fountain," +
                "european_castle,japanese_castle,classical_building,stadium,mountain_snow,camping,beach," +
                "desert,island,park,cityscape,city_sunset,city_dusk,night_with_stars,bridge_at_night,house," +
                "homes,house_with_garden,house_abandoned,construction_site,office,department_store,factory," +
                "post_office,european_post_office,hospital,bank,hotel,love_hotel,wedding,church," +
                "convenience_store,school,map"
            },

            objects_symbols: {
                icon: "computer",
                emoji: "watch,iphone,calling,computer,desktop,keyboard,trackball,printer,alarm_clock,clock," +
                "hourglass_flowing_sand,hourglass,camera,camera_with_flash,video_camera,movie_camera,projector," +
                "tv,microphone2,level_slider,control_knobs,radio,pager,joystick,telephone_receiver,telephone," +
                "fax,minidisc,floppy_disk,cd,dvd,vhs,battery,electric_plug,bulb,flashlight,candle,satellite," +
                "satellite_orbital,credit_card,money_with_wings,moneybag,gem,closed_umbrella,pouch,purse," +
                "handbag,briefcase,school_satchel,lipstick,eyeglasses,dark_sunglasses,womans_hat,sandal," +
                "high_heel,boot,mans_shoe,athletic_shoe,bikini,dress,kimono,womans_clothes,shirt,necktie," +
                "jeans,door,shower,bathtub,toilet,barber,syringe,pill,microscope,telescope,crystal_ball," +
                "wrench,knife,dagger,nut_and_bolt,hammer,tools,oil,bomb,smoking,gun,bookmark,newspaper," +
                "newspaper2,thermometer,label,key,key2,envelope,envelope_with_arrow,incoming_envelope,email," +
                "inbox_tray,outbox_tray,package,postal_horn,postbox,mailbox_closed,mailbox,mailbox_with_no_mail," +
                "mailbox_with_mail,page_facing_up,page_with_curl,bookmark_tabs,wastebasket,notepad_spiral," +
                "chart_with_upwards_trend,chart_with_downwards_trend,bar_chart,date,calendar,calendar_spiral," +
                "ballot_box,low_brightness,high_brightness,compression,frame_photo,scroll,clipboard,book," +
                "notebook,notebook_with_decorative_cover,ledger,closed_book,green_book,blue_book,orange_book," +
                "books,card_index,dividers,card_box,link,paperclip,paperclips,pushpin,scissors," +
                "triangular_ruler,round_pushpin,straight_ruler,triangular_flag_on_post,flag_white,flag_black," +
                "hole,file_folder,open_file_folder,file_cabinet,black_nib,pencil2,pen_ballpoint,pen_fountain," +
                "paintbrush,crayon,pencil,lock_with_ink_pen,closed_lock_with_key,lock,unlock,mega,loudspeaker," +
                // "speaker,sound,loud_sound,mute,zzz,bell,no_bell,cross_heavy,om_symbol,dove,thought_balloon," +
                "speaker,sound,loud_sound,mute,zzz,bell,no_bell,om_symbol,dove,thought_balloon," +
                "speech_balloon,anger_right,children_crossing,shield,mag,mag_right,speaking_head," +
                "sleeping_accommodation,no_entry_sign,no_entry,name_badge,no_pedestrians,do_not_litter," +
                "no_bicycles,non_potable_water,no_mobile_phones,underage,sparkle,eight_spoked_asterisk," +
                "negative_squared_cross_mark,white_check_mark,eight_pointed_black_star,vibration_mode," +
                "mobile_phone_off,vs,a,b,ab,cl,o2,sos,id,parking,wc,cool,free,new,ng,ok,up,atm," +
                "aries,taurus,gemini,cancer,leo,virgo,libra,scorpius,sagittarius,capricorn,aquarius," +
                "pisces,restroom,mens,womens,baby_symbol,wheelchair,potable_water,no_smoking," +
                "put_litter_in_its_place,arrow_forward,arrow_backward,arrow_up_small,arrow_down_small," +
                "fast_forward,rewind,arrow_double_up,arrow_double_down,arrow_right,arrow_left,arrow_up," +
                "arrow_down,arrow_upper_right,arrow_lower_right,arrow_lower_left,arrow_upper_left,arrow_up_down," +
                "left_right_arrow,arrows_counterclockwise,arrow_right_hook,leftwards_arrow_with_hook," +
                "arrow_heading_up,arrow_heading_down,twisted_rightwards_arrows,repeat,repeat_one,hash," +
                "zero,one,two,three,four,five,six,seven,eight,nine,ten,1234,abc,abcd,capital_abcd," +
                "information_source,signal_strength,cinema,symbols,heavy_plus_sign,heavy_minus_sign,wavy_dash," +
                "heavy_division_sign,heavy_multiplication_x,heavy_check_mark,arrows_clockwise,tm,copyright," +
                "registered,currency_exchange,heavy_dollar_sign,curly_loop,loop,part_alternation_mark," +
                "exclamation,question,grey_exclamation,grey_question,bangbang,interrobang,x,o,100,end," +
                "back,on,top,soon,cyclone,m,ophiuchus,six_pointed_star,beginner,trident,warning," +
                "hotsprings,rosette,recycle,anger,diamond_shape_with_a_dot_inside,spades,clubs,hearts," +
                "diamonds,ballot_box_with_check,white_circle,black_circle,radio_button,red_circle," +
                "large_blue_circle,small_red_triangle,small_red_triangle_down,small_orange_diamond," +
                "small_blue_diamond,large_orange_diamond,large_blue_diamond,black_small_square," +
                "white_small_square,black_large_square,white_large_square,black_medium_square,white_medium_square," +
                "black_medium_small_square,white_medium_small_square,black_square_button,white_square_button," +
                "clock1,clock2,clock3,clock4,clock5,clock6,clock7,clock8,clock9,clock10,clock11," +
                "clock12,clock130,clock230,clock330,clock430,clock530,clock630,clock730,clock830,clock930," +
                "clock1030,clock1130,clock1230"
            },

            flags: {
                icon: "jp",
                emoji: "au,at,be,br,ca,flag_cl,cn,co,dk,fi,fr,de,hk,in,flag_id,ie,il,it,jp,kr,mo," +
                "my,mx,nl,nz,no,ph,pl,pt,pr,ru,flag_sa,sg,za,es,se,ch,tr,gb,us,ae,vn,af,al,dz," +
                "ad,ao,ai,ag,ar,am,aw,ac,az,bs,bh,bd,bb,by,bz,bj,bm,bt,bo,ba,bw,bn,bg,bf,bi," +
                "kh,cm,cv,ky,cf,km,flag_cd,cg,td,cr,ci,hr,cu,cy,cz,dj,dm,do,tl,ec,eg,sv,gq,er," +
                "ee,et,fk,fo,fj,pf,ga,gm,ge,gh,gi,gr,gl,gd,gu,gt,gn,gw,gy,ht,hn,hu,is,ir,iq,jm," +
                "je,jo,kz,ke,ki,xk,kw,kg,la,lv,lb,ls,lr,ly,li,lt,lu,mk,mg,mw,mv,ml,mt,mh,mr,mu," +
                "fm,md,mc,mn,me,ms,ma,mz,mm,na,nr,np,nc,ni,ne,flag_ng,nu,kp,om,pk,pw,ps,pa,pg," +
                "py,pe,qa,ro,rw,sh,kn,lc,vc,ws,sm,st,sn,rs,sc,sl,sk,si,sb,so,lk,sd,sr,sz,sy,tw,tj," +
                "tz,th,tg,to,tt,tn,flag_tm,flag_tv,vi,ug,ua,uy,uz,vu,va,ve,wf,eh,ye,zm,zw"
            }
        }
    };

    var slice = [].slice,
        emojione = window.emojione,
        saveSelection, restoreSelection,
        emojioneList = {},
        eventStorage = {},
        setInterval = window.setInterval,
        clearInterval = window.clearInterval,
        readyCallbacks = [],
        uniRegexp,
        unique = 0;

    function emojioneReady(fn) {
        if (emojione) {
            fn();
        } else {
            readyCallbacks.push(fn);
        }
    };

    if (!emojione) {
        $.getScript("https://cdn.jsdelivr.net/emojione/1.5.0/lib/js/emojione.min.js", function () {
            emojione = window.emojione;
            var base = "https://cdnjs.cloudflare.com/ajax/libs/emojione/1.5.0/assets",
                sprite = base +"/sprites/emojione.sprites.css";
            emojione.imagePathPNG = base + "/png/";
            if (document.createStyleSheet) {
                document.createStyleSheet(sprite);
            } else {
                $('<link/>', {rel: 'stylesheet', href: sprite}).appendTo('head');
            }
            while (readyCallbacks.length) {
                readyCallbacks.shift().call();
            }
        });
    }

    emojioneReady(function() {
        $.each(emojione.emojioneList, function (shortname, keys) {
            // fix shortnames for emojione v1.5.0
            emojioneList[shortname.replace('-', '_')] = keys;
        });

        uniRegexp = new RegExp("<object[^>]*>.*?<\/object>|<span[^>]*>.*?<\/span>|<(?:object|embed|svg|img|div|span|p|a)[^>]*>|("+
            emojione.unicodeRegexp+")", "gi");
    });

    var EmojioneArea = function(element, options) {
        var self = this;
        eventStorage[self.id = ++unique] = {};
        emojioneReady(function() {
            init(self, element, options);
        });
    };

    EmojioneArea.prototype.on = function(events, handler) {
        if (events && $.isFunction(handler)) {
            var id = this.id;
            $.each(events.toLowerCase().split(' '), function(i, event) {
                (eventStorage[id][event] || (eventStorage[id][event] = [])).push(handler);
            });
        }
        return this;
    };

    EmojioneArea.prototype.off = function(events, handler) {
        if (events) {
            var id = this.id;
            $.each(events.toLowerCase().split(' '), function(i, event) {
                if (eventStorage[id][event] && !/^@/.test(event)) {
                    if (handler) {
                        $.each(eventStorage[id][event], function(j, fn) {
                            if (fn === handler) {
                                eventStorage[id][event] = eventStorage[id][event].splice(j, 1);
                            }
                        });
                    } else {
                        eventStorage[id][event] = [];
                    }
                }
            });
        }
        return this;
    };

    function trigger(self, event, args) {
        var result = true, j = 1;
        if (event) {
            event = event.toLowerCase();
            do {
                var _event = j==1 ? '@' + event : event;
                if (eventStorage[self.id][_event] && eventStorage[self.id][_event].length) {
                    $.each(eventStorage[self.id][_event], function (i, fn) {
                        return result = fn.apply(self, args|| []) !== false;
                    });
                }
            } while (result && !!j--);
        }
        return result;
    }

    function attach(self, element, events, target) {
        target = target || function (event, callerEvent) { return $(callerEvent.currentTarget) };

        $.each($.isArray(element) ? element : [element], function(i, el) {
            $.each(events, function(event, handler) {
                $(el).on(event = $.isArray(events) ? handler : event, function() {
                    var _target = $.isFunction(target) ? target.apply(self, [event].concat(slice.call(arguments))) : target;
                    if (_target) {
                        trigger(self, handler, [_target].concat(slice.call(arguments)));
                    }
                });
            });
        });
    }

    function htmlFromText(str, self) {
        str = str
            .replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/(?:\r\n|\r|\n)/g, '\n')
            .replace(/(\n+)/g, '<div>$1</div>')
            .replace(/\n/g, '<br/>')
            .replace(/<br\/><\/div>/g, '</div>');
        if (self.shortnames) {
            str = emojione.shortnameToUnicode(str);
        }
        return unicodeTo(str,
            '<img alt="{alt}" class="emojione' + (self.sprite ? '-{uni}" src="' + blankImg + '">' : '" src="{img}">'))
            .replace(/\t/g, '&nbsp;&nbsp;&nbsp;&nbsp;')
            .replace(/  /g, '&nbsp;&nbsp;');
    }

    function htmlFromText(str, self) {
        str = str
            .replace(/\t/g, '&nbsp;&nbsp;&nbsp;&nbsp;')
            .replace(/ /g, '&nbsp;')
            .replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/(?:\r\n|\r|\n)/g, '\n')
            .replace(/(\n+)/g, '<div>$1</div>')
            .replace(/\n/g, '<br/>')
            .replace(/<br\/><\/div>/g, '</div>');
        if (self.shortnames) {
            str = emojione.shortnameToUnicode(str);
        }
        return unicodeTo(str,
            '<img alt="{alt}" class="emojione' + (self.sprite ? '-{uni}" src="' + blankImg + '">' : '" src="{img}">'));
    }

    function textFromHtml(str, self) {
        str = str
            .replace(/<img[^>]*alt="([^"]+)"[^>]*>/ig, '$1')
            .replace(/\n|\r/g, '')
            .replace(/<br[^>]*>/ig, '\n')
            .replace(/(?:<(?:div|p|ol|ul|li|pre|code|object)[^>]*>)+/ig, '<div>')
            .replace(/(?:<\/(?:div|p|ol|ul|li|pre|code|object)>)+/ig, '</div>')
            .replace(/\n<div><\/div>/ig, '\n')
            .replace(/<div><\/div>\n/ig, '\n')
            .replace(/(?:<div>)+<\/div>/ig, '\n')
            .replace(/([^\n])<\/div><div>/ig, '$1\n')
            .replace(/(?:<\/div>)+/ig, '</div>')
            .replace(/([^\n])<\/div>([^\n])/ig, '$1\n$2')
            .replace(/<\/div>/ig, '')
            .replace(/([^\n])<div>/ig, '$1\n')
            .replace(/\n<div>/ig, '\n')
            .replace(/<div>\n/ig, '\n\n')
            .replace(/<(?:[^>]+)?>/g, '')
            .replace(/&nbsp;/g, ' ')
            .replace(/\x20*&nbsp;\x20*/g, '&nbsp;')
            .replace(/&gt;/g, '>')
            .replace(/&lt;/g, '<');
        return self && self.shortnames ? emojione.toShort(str) : str;
    }

    EmojioneArea.prototype.setText = function(str) {
        var self = this, args = arguments;
        emojioneReady(function() {
            self.editor.html(htmlFromText(str, self));
            self.content = self.editor.html();
            if (args.length === 1) {
                trigger(self, 'change', [self.editor]);
            }
        });
    }

    EmojioneArea.prototype.getText = function() {
        return textFromHtml(this.editor.html(), this);
    }

    function getTemplate(template, unicode, shortname) {
        return template
            .replace('{name}', shortname || '')
            .replace('{img}', emojione.imagePathPNG + unicode + '.png'/* + emojione.cacheBustParam*/)
            .replace('{uni}', unicode)
            .replace('{alt}', emojione.convert(unicode));
    }

    function unicodeTo(str, template) {
        return str.replace(uniRegexp, function(unicodeChar) {
            if (typeof unicodeChar !== 'undefined' && unicodeChar in emojione.jsEscapeMap) {
                return getTemplate(template, emojione.jsEscapeMap[unicodeChar]);
            }
            return unicodeChar;
        });
    }

    function shortnameTo(str, template) {
        return str.replace(/:?[\w_]+:?/g, function(shortname) {
            shortname = ":" + shortname.replace(/:$/,'').replace(/^:/,'') + ":";
            if (shortname in emojioneList) {
                return getTemplate(template, emojioneList[shortname][emojioneList[shortname].length-1].toUpperCase(), shortname);
            }
            return shortname;
        });
    };

    function init(self, source, options) {
        options = $.extend({}, default_options, options);

        var sourceValFunc = source.is("TEXTAREA") || source.is("INPUT") ? "val" : "text",
            app = options.template,
            stayFocused = false,
            container = !!options.container ? $(options.container) : false,
            editor, filters, tabs, scrollArea, filtersBtns, filtersArrowLeft, filtersArrowRight,
            filtersWidth, scrollLeft = 0, scrollAreaWidth = 0, filterWidth,
            resizeHandler = function() {
                var width = filters.width();
                if (width !== filtersWidth) {
                    filtersWidth = width;
                    trigger(self, 'resize', [editor]);
                }
            }, resizeHandlerID;

        self.sprite = options.useSprite;
        self.shortnames = options.shortnames;

        for (var el = ["editor", "filters", "tabs"], i=0; i<3; i++) {
            app = app.replace(new RegExp('<' + el[i] + '/?>' ,'i'), '<div class="emojionearea-' + el[i] + '"></div>');
        }

        app = $('<div/>', {"class" : source.attr("class"), role: "application"}).addClass("emojionearea").html(app);
        editor = self.editor = app.find(".emojionearea-editor")
            .attr({
                contenteditable: true,
                placeholder: options["placeholder"] || source.data("placeholder") || source.attr("placeholder") || "",
                tabindex: 0
            });

        for (var attr = ["dir", "spellcheck", "autocomplete", "autocorrect", "autocapitalize"], i=0; i<5; i++) {
            editor.attr(attr[i], options[attr[i]]);
        }

        filters = app.find(".emojionearea-filters");
        if (options.autoHideFilters) {
            filters.hide();
        }

        tabs = app.find(".emojionearea-tabs");

        $.each(options.filters, function(filter, params) {
            $("<i/>", {"class": "emojionearea-filter", "data-filter": filter})
                .wrapInner(shortnameTo(params.icon, self.sprite ? '<i class="emojione-{uni}"/>' : '<img class="emojione" src="{img}"/>'))
                .appendTo(filters);
            $("<div/>", {"class": "emojionearea-tab emojionearea-tab-" + filter}).hide()
                .data("items", shortnameTo(params.emoji, '<i class="emojibtn" role="button">' +
                    (self.sprite ? '<i class="emojione-{uni}"' : '<img class="emojione" src="{img}"') +
                    ' data-name="{name}"/></i>'))
                .appendTo(tabs);
        });

        filters.wrapInner('<div class="emojionearea-filters-scroll"/>');
        filtersArrowLeft = $('<i class="emojionearea-filter-arrow-left"/>', {role: "button"}).appendTo(filters);
        filtersArrowRight = $('<i class="emojionearea-filter-arrow-right"/>', {role: "button"}).appendTo(filters);

        filtersBtns = filters.find(".emojionearea-filter");
        scrollArea = filters.children(".emojionearea-filters-scroll");

        if (!!container) {
            container.wrapInner(app);
        } else {
            app.insertAfter(source);
        }

        if (options.hideSource) {
            source.hide();
        }

        self.setText(source[sourceValFunc]());

        attach(self, [filters, tabs], {mousedown: "area.mousedown"}, editor);
        attach(self, editor, ["paste"], editor);
        attach(self, editor, ["focus", "blur"], function() { return !!stayFocused ? false : editor; });
        attach(self, [editor, filters, tabs], ["mousedown", "mouseup", "click", "keyup", "keydown"], editor);
        attach(self, filters.find(".emojionearea-filter"), {click: "filter.click"});
        attach(self, filtersArrowLeft, {click: "arrowLeft.click"});
        attach(self, filtersArrowRight, {click: "arrowRight.click"});

        function scrollFilters() {
            if (!scrollAreaWidth) {
                $.each(filtersBtns, function (i, e) {
                    scrollAreaWidth += $(e).outerWidth(true);
                });
                filterWidth = filtersBtns.eq(0).outerWidth(true);
            }
            if (scrollAreaWidth > filtersWidth) {
                filtersArrowRight.addClass("active");
                filtersArrowLeft.addClass("active");

                if (scrollLeft + scrollAreaWidth <= filtersWidth) {
                    scrollLeft = filtersWidth - scrollAreaWidth;
                    filtersArrowRight.removeClass("active");
                } else if (scrollLeft >= 0) {
                    scrollLeft = 0;
                    filtersArrowLeft.removeClass("active");
                }
                scrollArea.css("left", scrollLeft);
            } else {
                if (scrollLeft !== 0) {
                    scrollLeft = 0;
                    scrollArea.css("left", scrollLeft);
                }
                filtersArrowRight.removeClass("active");
                filtersArrowLeft.removeClass("active");
            }
        }


        self.on("@filter.click", function(element) {
                if (element.is(".active")) {
                    element.removeClass("active");
                    tabs.children().hide();
                } else {
                    filtersBtns.filter(".active").removeClass("active");
                    element.addClass("active");
                    var i, timer, tab = tabs.children().hide()
                        .filter(".emojionearea-tab-" + element.data("filter")).show(),
                        items = tab.data("items"),
                        event = {click: "emojibtn.click"};
                    if (items) {
                        tab.data("items", false);
                        items = items.split(',');
                        if (self.sprite) {
                            tab.html(items.join(''));
                            attach(self, tab.find(".emojibtn"), event);
                        } else {
                            timer = setInterval(function () {
                                for (i = 0; i < 20 && items.length; i++) {
                                    tab.append(items.shift());
                                    attach(self, tab.find(".emojibtn").not(".handled").addClass("handled"), event);
                                }
                                if (!items.length) clearInterval(timer);
                            }, 5);
                        }
                    }
                }
            })

            .on("@resize", function() {
                scrollFilters();
            })

            .on("@arrowLeft.click", function() {
                scrollLeft += filterWidth;
                scrollFilters();
            })

            .on("@arrowRight.click", function() {
                scrollLeft -= filterWidth;
                scrollFilters();
            })

            .on("@paste", function(element) {
                stayFocused = true;
                pasteHtmlAtCaret('<span> </span>');

                var sel = saveSelection(element[0]),
                    editorScrollTop = element.scrollTop(),
                    clipboard = $("<div/>", {contenteditable: true})
                        .css({position: "fixed", left: "-999px", width: "1px", height: "1px", top: "20px", overflow: "hidden"})
                        .appendTo($("BODY"))
                        .focus();

                window.setTimeout(function() {
                    var caretID = "caret-" + (new Date()).getTime();
                    element.focus();
                    restoreSelection(element[0], sel);
                    pasteHtmlAtCaret(htmlFromText(textFromHtml(clipboard.html().replace(/\r\n|\n|\r/g, '<br>'), self), self));
                    clipboard.remove();
                    pasteHtmlAtCaret('<i id="' + caretID +'"></i>');
                    element.scrollTop(editorScrollTop);
                    var caret = $("#" + caretID),
                        top = caret.offset().top - element.offset().top,
                        height = element.height();
                    if (editorScrollTop + top >= height || editorScrollTop > top) {
                        element.scrollTop(editorScrollTop + top - 2 * height/3);
                    }
                    caret.remove();
                    stayFocused = false;
                }, 200);
            })

            .on("@emojibtn.click", function(element) {
                saveSelection(editor[0]);
                pasteHtmlAtCaret(shortnameTo(element.children().data("name"),
                    '<img alt="{alt}" class="emojione' + (self.sprite ? '-{uni}" src="'+blankImg+'">' : '" src="{img}">')));
            })

            .on("@area.mousedown", function(element, event) {
                if (!options.autoHideFilters && !app.is(".focused")) {
                    element.focus();
                }
                event.preventDefault();
                return false;
            })

            .on("@change", function(element) {
                var html = element.html().replace(/<\/?(?:div|span|p)[^>]*>/ig, '');
                // clear input, fix: chrome add <br> on contenteditable is empty
                if (!html.length || /^<br[^>]*>$/i.test(html)) {
                    self.setText('', false);
                }
                source[sourceValFunc](self.getText());
            })

            .on("@focus", function() {
                resizeHandler();
                resizeHandlerID = setInterval(resizeHandler, 500);
                app.addClass("focused");
                if (options.autoHideFilters) {
                    filters.slideDown(400);
                }
            })

            .on("@blur", function(element) {
                scrollLeft = 0;
                scrollFilters();
                app.removeClass("focused");
                clearInterval(resizeHandlerID);
                if (options.autoHideFilters) {
                    filters.slideUp(400);
                }
                //tabs.children().hide();
                var content = element.html();
                if (self.content !== content) {
                    self.content = content;
                    trigger(self, 'change', [editor]);
                    source.blur().trigger("change");
                } else {
                    source.blur();
                }
            });
    };


    $.fn.emojioneArea = function(options) {
        return this.each(function() {
            if (!!this.emojioneArea) return this.emojioneArea;
            return this.emojioneArea = new EmojioneArea($(this), options);
        });
    };


    if (window.getSelection && document.createRange) {
        saveSelection = function(el) {
            var range = window.getSelection().getRangeAt(0);
            var preSelectionRange = range.cloneRange();
            preSelectionRange.selectNodeContents(el);
            preSelectionRange.setEnd(range.startContainer, range.startOffset);
            return preSelectionRange.toString().length;
        };

        restoreSelection = function(el, sel) {
            var charIndex = 0, range = document.createRange();
            range.setStart(el, 0);
            range.collapse(true);
            var nodeStack = [el], node, foundStart = false, stop = false;

            while (!stop && (node = nodeStack.pop())) {
                if (node.nodeType == 3) {
                    var nextCharIndex = charIndex + node.length;
                    if (!foundStart && sel >= charIndex && sel <= nextCharIndex) {
                        range.setStart(node, sel - charIndex);
                        range.setEnd(node, sel - charIndex);
                        stop = true;
                    }
                    charIndex = nextCharIndex;
                } else {
                    var i = node.childNodes.length;
                    while (i--) {
                        nodeStack.push(node.childNodes[i]);
                    }
                }
            }

            sel = window.getSelection();
            sel.removeAllRanges();
            sel.addRange(range);
        }
    } else if (document.selection && document.body.createTextRange) {
        saveSelection = function(el) {
            var selectedTextRange = document.selection.createRange(),
                preSelectionTextRange = document.body.createTextRange();
            preSelectionTextRange.moveToElementText(el);
            preSelectionTextRange.setEndPoint("EndToStart", selectedTextRange);
            var start = preSelectionTextRange.text.length;

            return [start, start + selectedTextRange.text.length];
        };

        restoreSelection = function(el, sel) {
            var textRange = document.body.createTextRange();
            textRange.moveToElementText(el);
            textRange.collapse(true);
            textRange.moveEnd("character", sel[1]);
            textRange.moveStart("character", sel[0]);
            textRange.select();
        };
    }

    function pasteHtmlAtCaret(html) {
        var sel, range;
        if (window.getSelection) {
            sel = window.getSelection();
            if (sel.getRangeAt && sel.rangeCount) {
                range = sel.getRangeAt(0);
                range.deleteContents();
                var el = document.createElement("div");
                el.innerHTML = html;
                var frag = document.createDocumentFragment(), node, lastNode;
                while ( (node = el.firstChild) ) {
                    lastNode = frag.appendChild(node);
                }
                range.insertNode(frag);
                if (lastNode) {
                    range = range.cloneRange();
                    range.setStartAfter(lastNode);
                    range.collapse(true);
                    sel.removeAllRanges();
                    sel.addRange(range);
                }
            }
        } else if (document.selection && document.selection.type != "Control") {
            document.selection.createRange().pasteHTML(html);
        }
    }

}) (document, window, jQuery);