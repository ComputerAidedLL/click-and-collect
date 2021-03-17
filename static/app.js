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
    var $table = $("<table>");
    var $td = $("<td>");
    if ('hyp' in proofAsJson) {
        $td.append(createFormulas(proofAsJson['hyp']));
    }
    $td.append($('<span class="turnstile explained">⊢</span>'));
    if ('cons' in proofAsJson) {
        $td.append(createFormulas(proofAsJson['cons']));
    }
    $table.append($td);
    $div2.append($table);
    $div.append($div2);
    proofdiv.append($div);
}

function createFormulas(formulasAsJson) {
    var $ul = $("<ul>", {"class": "commaList"});
    for (var i = 0; i < formulasAsJson.length; i++) {
        var $li = $("<li>");
        var $span = $("<span>", {"class": "junct"}).html(createFormula(formulasAsJson[i]));
        $li.append($span);
        $ul.append($li);
    }
    return $ul;
}

const UNARY_OPERATORS = {
    "negation": '<span>¬</span>',
    "ofcourse": '<span>!</span>',
    "whynot": '<span>?</span>'
};

const BINARY_OPERATORS = {
    "implication": '<span class="binary-operator">→</span>',
    "conjunction": '<span class="binary-operator">∧</span>',
    "disjunction": '<span class="binary-operator">∨</span>',
    "tensor": '<span class="binary-operator">⊗</span>',
    "par": '<span class="binary-operator flip">&</span>',
    "with": '<span class="binary-operator">&</span>',
    "plus": '<span class="binary-operator">⊕</span>',
    "lollipop": '<span class="binary-operator">⊸</span>'
};

const NEUTRAL_ELEMENTS = {
    "true": '<span class="neutral-element">true</span>',
    "false": '<span class="neutral-element">false</span>',
    "one": '<span class="neutral-element">1</span>',
    "bottom": '<span class="neutral-element">⊥</span>',
    "top": '<span class="binary-operator">T</span>',
    "zero": '<span class="binary-operator">0</span>'
};



function createFormula(formulaAsJson, needParentheses = false) {
    switch (formulaAsJson.type) {
        case "litteral":
            return formulaAsJson.value;

        case "neutral":
            return NEUTRAL_ELEMENTS[formulaAsJson.value];

        case "negation":
        case "ofcourse":
        case "whynot":
            return UNARY_OPERATORS[formulaAsJson.type] + createFormula(formulaAsJson.value, true);

        case "orthogonal":
            return createFormula(formulaAsJson.value, true)
                + '<span class="orthogonal">⊥</span>';

        case "implication":
        case "conjunction":
        case "disjunction":
        case "tensor":
        case "par":
        case "with":
        case "plus":
        case "lollipop":
            let formula = createFormula(formulaAsJson.value1, true)
                + '<span class="binary-operator">' + BINARY_OPERATORS[formulaAsJson.type] + '</span>'
                + createFormula(formulaAsJson.value2, true);
            return addParentheses(formula, needParentheses);

        default:
            console.error('No display rule for type ' + formulaAsJson.type);
    }
}

function addParentheses(formula, needParentheses) {
    if (needParentheses) {
        return '(' + formula + ')';
    }

    return  formula;
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