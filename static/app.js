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

const RULES = {
    "axiom": '<span class="rule">ax</span>',
    "tensor": '<span class="rule">⊗</span>',
    "par": '<span class="rule flip">&</span>',
    "with": '<span class="rule">&</span>',
    "plus_left": '<span class="rule">⊕1</span>',
    "plus_right": '<span class="rule">⊕2</span>',
    "one": '<span class="rule">1</span>',
    "bottom": '<span class="rule">⊥</span>',
    "top": '<span class="rule">⊤</span>',
    "promotion": '<span class="rule">!</span>',
    "dereliction": '<span class="rule">?d</span>',
    "contraction": '<span class="rule">?c</span>',
    "weakening": '<span class="rule">?w</span>'
};

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

function addSequentListPremisses($td, sequentList, rule) {
    // Add line
    $td.addClass("inference");

    // Add rule symbol
    $td.next('.tagBox').html(getRuleSymbol(rule));

    // Add new sequents
    let $table = $td.closest('table');
    if (sequentList.length === 0) {
        // Do nothing
    } else if (sequentList.length === 1) {
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

function getRuleSymbol(rule) {
    return $('<div>', {'class': 'tag'})
        .html(RULES[rule]);
}

function createSequent(sequentAsJson) {
    let $table = $("<table>");
    let $td = $("<td>").data('sequent', sequentAsJson);
    if ('hyp' in sequentAsJson) {
        createFormulas(sequentAsJson, 'hyp', $td);
    }
    $td.append($('<span class="turnstile explained">⊢</span>'));
    if ('cons' in sequentAsJson) {
        createFormulas(sequentAsJson, 'cons', $td);
    }
    $table.append($td);
    let $tagBox = $("<td>", {"class": "tagBox"})
        .html('&nbsp;');
    $table.append($tagBox);
    return $table;
}

function createFormulas(sequentAsJson, field, $td) {
    let $ul = $("<ul>", {"class": ["commaList " + field]}).sortable();
    for (let i = 0; i < sequentAsJson[field].length; i++) {
        let formulaAsJson = sequentAsJson[field][i];
        let $li = $("<li>").data('initialPosition', i);
        let $span = $("<span>", {"class": "junct"})
            .html(createFormula(formulaAsJson));
        let possibleRules = getRules(formulaAsJson);
        for (let j = 0; j < possibleRules.length; j++) {
            $span.click(applyRule(possibleRules[j], $li));
        }
        $li.append($span);
        $ul.append($li);
    }
    $td.append($ul);
}

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

function getRules(formulaAsJson) {
    switch (formulaAsJson.type) {
        case "litteral":
        case "orthogonal":
            return ["axiom"];

        case "tensor":
        case "par":
            return [formulaAsJson.type];

        default:
            return [];
    }
}

function getSequentWithPermutations($td) {
    let sequent = $td.data('sequent');

    return {
        'hyp': getFormulasWithPermutation($td.find('ul.hyp'), sequent['hyp']),
        'cons': getFormulasWithPermutation($td.find('ul.cons'), sequent['cons'])
    };
}

function getFormulasWithPermutation($ul, initialFormulas) {
    let newFormulas = [];

    $ul.find('li').each(function(i, obj) {
        let initialPosition = $(obj).data('initialPosition');
        newFormulas.push(initialFormulas[initialPosition]);
    })

    return newFormulas;
}

function applyRule(rule, $li) {
    return function() {
        let $td = $li.closest('td');
        let formulaPosition = $li.parent().children().index($li);
        let newSequent = getSequentWithPermutations($td);

        $.ajax({
            type: "POST",
            url: '/apply_rule',
            contentType:"application/json; charset=utf-8",
            data: JSON.stringify({
                'rule': rule,
                'sequent': newSequent,
                'formulaPosition': formulaPosition
            }),
            success: function(data)
            {
                console.log(data);
                addSequentListPremisses($td, data, rule);
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