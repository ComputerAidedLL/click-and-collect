$( function() {
    $("#usrform").submit(function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    $( ".sortable" ).sortable()
        .disableSelection();

    $("body").append("<div id=\"dialog-form-options\" title=\"Select option\">\n" +
        "                <form>\n" +
        "                     <input type=\"radio\" id=\"double_a\" name=\"formula\" value=\"double_a\">\n" +
        "                     <label for=\"double_a\">A, A</label><br>\n" +
        "                     <input type=\"radio\" id=\"double_neg\" name=\"formula\" value=\"double_neg\">\n" +
        "                     <label for=\"double_neg\">¬¬A</label><br>\n" +
        "                     <input type=\"radio\" id=\"nothing\" name=\"formula\" value=\"nothing\">\n" +
        "                     <label for=\"nothing\">∅</label>\n" +
        "                </form>\n" +
        "            </div>")
        .append("<div id=\"dialog-form-text\" title=\"Enter value for x\">\n" +
            "                <form>\n" +
            "                     <input type=\"text\" />\n" +
            "                </form>\n" +
            "            </div>");
    let dialogoptions = $("#dialog-form-options").dialog({
        autoOpen: false,
        modal: true,
        buttons: {
            "OK": function () {
                dialogoptions.dialog("close");
            },
            Cancel: function () {
                dialogoptions.dialog("close");
            }
        },
        close: function () {
        }
    });

    let dialogtext = $("#dialog-form-text").dialog({
        autoOpen: false,
        modal: true,
        buttons: {
            "OK": function () {
                dialogtext.dialog("close");
            },
            Cancel: function () {
                dialogtext.dialog("close");
            }
        },
        close: function () {
        }
    });
} );

function submitproof(element) {
    cleanproof();

    var form = $(element).closest('form');
    var url = '/parse_proof_string';

    $.ajax({
           type: "GET",
           url: url,
           data: {
                'proofAsString': form.find('textarea[name=proofAsString]').val()
           },
           success: function(data)
           {
                if (data.is_valid) {
                    updateproof(data.proof_as_json);
                } else {
                    alert(data.error_message);
                }
           }
     });
}

function cleanproof() {
    $('#main-proof').html('');
}

function updateproof(proofAsJson) {
    console.log(proofAsJson);
    var proofdiv = $('#main-proof');

    var $div = $("<div>", {"class": "proofIsIncomplete"});
    var $div2 = $("<div>", {"class": "proof"});
    if ('hyp' in proofAsJson) {
        $div2.append(createFormulas(proofAsJson['hyp']));
    }
    $div2.append($('<span class="turnstile explained">⊢</span>'));
    if ('cons' in proofAsJson) {
        $div2.append(createFormulas(proofAsJson['cons']));
    }
    $div.append($div2);
    proofdiv.append($div);
}

function createFormulas(formulasAsJson) {
    var $ul = $("<ul>", {"class": "commaList"});
    for (var i = 0; i < formulasAsJson.length; i++) {
        var $li = $("<li>");
        var $span = $("<span>", {"class": "junct"}).text(formulasAsJson[i]);
        $li.append($span);
        $ul.append($li);
    }
    return $ul;
}

function validate(element) {
    let p = $(element).closest('.proofIsIncomplete');
    p.removeClass('proofIsIncomplete');
    p.addClass('proofIsDone');

    let q = $(element).closest('td');
    q.addClass('inference');
}

function reset(element) {
    let p = $(element).closest('.proofIsDone');
    p.removeClass('proofIsDone');
    p.addClass('proofIsIncomplete');

    let q = $(element).closest('td');
    q.removeClass('inference');
}

function openpopupoptions() {
    $("#dialog-form-options").dialog( "open" );
}

function openpopuptext() {
    $("#dialog-form-text").dialog( "open" );
}

function moveleft(element) {
    let ul = $(element).closest('div').find('ul');
    ul.find('li').first().insertAfter(ul.find('li').last());
}

function moveright(element) {
    let ul = $(element).closest('div').find('ul');
    ul.find('li').last().insertBefore(ul.find('li').first());
}