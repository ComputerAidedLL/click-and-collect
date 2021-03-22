$( function() {
    // SEQUENT FORM
    $("#usrform").submit(function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    // SORTABLE FORMULA LIST
    $( ".sortable" ).sortable()
        .disableSelection();

    // *****
    // POPUP
    // *****

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

// ************
// SEQUENT FORM
// ************

function submitSequent(element) {
    cleanSequentInput();

    let form = $(element).closest('form');
    let url = '/parse_sequent';

    $.ajax({
           type: "GET",
           url: url,
           data: {
                'sequentAsString': form.find('input[name=sequentAsString]').val()
           },
           success: function(data)
           {
                if (data.is_valid) {
                    initProof(data.sequent_as_json);
                } else {
                    alert(data.error_message);
                }
           }
     });
}

function cleanSequentInput() {
    $('#main-proof-container').html('');
}

// *************
// PROOF DISPLAY
// *************

function initProof(sequentAsJson) {
    console.log(sequentAsJson);
    let proofdiv = $('#main-proof-container');

    let $div = $("<div>", {"class": "proofIsIncomplete"});
    let $div2 = $("<div>", {"class": "proof"});
    $div2.append(createSequent(sequentAsJson));
    $div.append($div2);
    proofdiv.append($div);
}

function addSequentListPremisses($td, sequentList) {
    $td.addClass("inference");
    let $table = $td.closest('table');
    if (sequentList.length === 1) {
        createSequent(sequentList[0]).insertBefore($table);
    } else {
        let $div = $("<div>");
        for (let i = 0; i < sequentList.length; i++) {
            let $sibling = $("<div>", {"class": "sibling"})
            $sibling.append(createSequent(sequentList[i]))
            $div.append($sibling);
        }
        $div.insertBefore($table);
    }
}

function createSequent(sequentAsJson) {
    let $table = $("<table>");
    let $td = $("<td>");
    if ('hyp' in sequentAsJson) {
        $td.append(createFormulas(sequentAsJson, sequentAsJson['hyp'], $td));
    }
    $td.append($('<span class="turnstile explained">⊢</span>'));
    if ('cons' in sequentAsJson) {
        $td.append(createFormulas(sequentAsJson, sequentAsJson['cons'], $td));
    }
    $table.append($td);
    let $tagBox = $("<td>", {"class": "tagBox"})
        .html('&nbsp;');
    $table.append($tagBox);
    return $table;
}

function createFormulas(sequentAsJson, formulasAsJson, $td) {
    let $ul = $("<ul>", {"class": "commaList"});
    for (let i = 0; i < formulasAsJson.length; i++) {
        let formulaAsJson = formulasAsJson[i];
        let $li = $("<li>");
        let $span = $("<span>", {"class": "junct"})
            .html(createFormula(formulaAsJson))
            .click(applyRule(formulaAsJson, sequentAsJson, i, $td));
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
    "top": '<span class="neutral-element">⊤</span>',
    "zero": '<span class="neutral-element">0</span>'
};

function createFormula(formulaAsJson, isMainFormula = true) {
    switch (formulaAsJson.type) {
        case "litteral":
            return formulaAsJson.value;

        case "neutral":
            return NEUTRAL_ELEMENTS[formulaAsJson.value];

        case "negation":
        case "ofcourse":
        case "whynot":
            return UNARY_OPERATORS[formulaAsJson.type] + createFormula(formulaAsJson.value, false);

        case "orthogonal":
            return createFormula(formulaAsJson.value, false)
                + '<span class="exponent">⊥</span>';

        case "implication":
        case "conjunction":
        case "disjunction":
        case "tensor":
        case "par":
        case "with":
        case "plus":
        case "lollipop":
            let classField = 'binary-operator';
            if (isMainFormula) {
                classField += ' primaryConnective';
            }
            let formula = createFormula(formulaAsJson.value1, false)
                + '<span class="' + classField + '">'
                + BINARY_OPERATORS[formulaAsJson.type]
                + '</span>'
                + createFormula(formulaAsJson.value2, false);
            return addParentheses(formula, isMainFormula);

        default:
            console.error('No display rule for type ' + formulaAsJson.type);
    }
}

function addParentheses(formula, isMainFormula) {
    if (!isMainFormula) {
        return '(' + formula + ')';
    }

    return  formula;
}

// **********
// OPERATIONS
// **********

function applyRule(formulaAsJson, sequentAsJson, formulaPosition, $td) {
    return function() {
        let url = '/apply_rule';
        $.ajax({
            type: "POST",
            url: url,
            contentType:"application/json; charset=utf-8",
            data: JSON.stringify({
                'rule': formulaAsJson.type,
                'sequent': sequentAsJson,
                'formulaPosition': formulaPosition
            }),
            success: function(data)
            {
                console.log(data);
                addSequentListPremisses($td, data);
            },
            error: function(XMLHttpRequest) {
                alert(XMLHttpRequest.responseText);
            }
        });
    }
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

// *****
// POPUP
// *****

function openpopupoptions() {
    $("#dialog-form-options").dialog( "open" );
}

function openpopuptext() {
    $("#dialog-form-text").dialog( "open" );
}

// ************************
// MOVE WITHIN FORMULA LIST
// ************************

function moveleft(element) {
    let ul = $(element).closest('div').find('ul');
    ul.find('li').first().insertAfter(ul.find('li').last());
}

function moveright(element) {
    let ul = $(element).closest('div').find('ul');
    ul.find('li').last().insertBefore(ul.find('li').first());
}