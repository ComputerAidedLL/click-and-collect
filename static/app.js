const UNARY_OPERATORS = {
    'negation': '<span>¬</span>',
    'ofcourse': '<span>!</span>',
    'whynot': '<span>?</span>'
};

const BINARY_OPERATORS = {
    'implication': '<span class="binary-operator">→</span>',
    'conjunction': '<span class="binary-operator">∧</span>',
    'disjunction': '<span class="binary-operator">∨</span>',
    'tensor': '<span class="binary-operator">⊗</span>',
    'par': '<span class="binary-operator flip">&</span>',
    'with': '<span class="binary-operator">&</span>',
    'plus': '<span class="binary-operator">⊕</span>',
    'lollipop': '<span class="binary-operator">⊸</span>'
};

const NEUTRAL_ELEMENTS = {
    'true': '<span class="neutral-element">true</span>',
    'false': '<span class="neutral-element">false</span>',
    'one': '<span class="neutral-element">1</span>',
    'bottom': '<span class="neutral-element">⊥</span>',
    'top': '<span class="neutral-element">⊤</span>',
    'zero': '<span class="neutral-element">0</span>'
};

const RULES = {
    'axiom': '<span class="rule italic">ax</span>',
    'tensor': '<span class="rule">⊗</span>',
    'par': '<span class="rule flip">&</span>',
    'with': '<span class="rule">&</span>',
    'plus_left': '<span class="rule">⊕<span class="index">1</span></span>',
    'plus_right': '<span class="rule">⊕<span class="index">2</span></span>',
    'one': '<span class="rule">1</span>',
    'bottom': '<span class="rule">⊥</span>',
    'top': '<span class="rule">⊤</span>',
    // rule zero does not exist
    'promotion': '<span class="rule">!</span>',
    'dereliction': '<span class="rule">?d</span>',
    'contraction': '<span class="rule">?c</span>',
    'weakening': '<span class="rule">?w</span>'
};

$( function() {
    // SEQUENT FORM
    $('#sequent-form').on('submit', function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    // SORTABLE FORMULA LIST
    $( '.sortable' ).sortable()
        .disableSelection();

    // *****
    // POPUP
    // *****

    $('body').append("<div id=\"dialog-form-options\" title=\"Select option\">\n" +
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
    let dialogoptions = $('#dialog-form-options').dialog({
        autoOpen: false,
        modal: true,
        buttons: {
            'OK': function () {
                dialogoptions.dialog('close');
            },
            Cancel: function () {
                dialogoptions.dialog('close');
            }
        },
        close: function () {
        }
    });

    let dialogtext = $('#dialog-form-text').dialog({
        autoOpen: false,
        modal: true,
        buttons: {
            'OK': function () {
                dialogtext.dialog('close');
            },
            Cancel: function () {
                dialogtext.dialog('close');
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
    let sequentAsString = form.find($('input[name=sequentAsString]')).val();
    let url = '/parse_sequent';

    $.ajax({
        type: 'GET',
        url: url,
        data: {
            'sequentAsString': sequentAsString
        },
        success: function(data)
        {
            if (data['is_valid']) {
                initProof(data['sequent_as_json']);
            } else {
                displayPedagogicError(data['error_message']);
            }
        },
        error: function(jqXHR, textStatus, errorThrown) {
            console.log(jqXHR);
            console.log(jqXHR.responseText);
            console.log(textStatus);
            console.log(errorThrown);
            alert('Technical error, check browser console for more details.');
        }
     });
}

function cleanSequentInput() {
    $('#main-proof-container').html('');
}

// ***************
// PEDAGOGIC ERROR
// ***************

function displayPedagogicError(errorMessage) {
    let $mainContainer = $('#main-proof-container');
    let $div = $mainContainer
        .children('div.pedagogic-error');
    if (!$div.length) {
        $div = $('<div>', {'class': 'pedagogic-error'});
        $div.append($('<div>', {'class': 'message'}));
        let $close = $('<div>', {'class': 'close-button'});
        $close.html('✖');
        $close.on('click', function () {cleanPedagogicError();});
        $div.append($close);
        $mainContainer.append($div);
    }
    $div.children('div.message').text(errorMessage);
}

function cleanPedagogicError() {
    $('#main-proof-container')
        .children('div.pedagogic-error')
        .remove();
}

// *************
// PROOF DISPLAY
// *************

function initProof(sequentAsJson) {
    console.log(sequentAsJson);
    let proofdiv = $('#main-proof-container');

    let $div = $('<div>', {'class': 'proofIsIncomplete'});
    let $div2 = $('<div>', {'class': 'proof'});
    $div2.append(createSequent(sequentAsJson));
    $div.append($div2);
    proofdiv.append($div);
}

function createSequent(sequentAsJson) {
    let $table = $('<table>');
    let $td = $('<td>', {'class': 'sequent'})
        .data('sequent', sequentAsJson);
    if ('hyp' in sequentAsJson) {
        createFormulas(sequentAsJson, 'hyp', $td);
    }
    let $thesisSpan = $('<span class="turnstile explained">⊢</span>');
    $thesisSpan.on('click', function () {applyRule('axiom', $td, 0);});
    $td.append($thesisSpan);
    if ('cons' in sequentAsJson) {
        createFormulas(sequentAsJson, 'cons', $td);
    }
    $table.append($td);
    let $tagBox = $('<td>', {'class': 'tagBox'})
        .html('&nbsp;');
    $table.append($tagBox);
    return $table;
}

function createFormulas(sequentAsJson, field, $td) {
    let $ul = $('<ul>', {'class': ['commaList ' + field]}).sortable();
    for (let i = 0; i < sequentAsJson[field].length; i++) {
        let formulaAsJson = sequentAsJson[field][i];
        let $li = $('<li>').data('initialPosition', i);

        // Build formula
        let $span = $('<span>', {'class': 'main-formula'})
            .html(createFormulaHTML(formulaAsJson, true));
        $li.append($span);

        // Add event (click, ...)
        let possibleRules = getRules(formulaAsJson);
        for (let ruleEvent of possibleRules) {
            let $spanForEvent = $li.find('span.' + ruleEvent.element).first();
            $spanForEvent.on(ruleEvent.event, buildApplyRuleCallBack(ruleEvent.rule, $li));
            $spanForEvent.addClass('primaryExpr');
        }

        $ul.append($li);
    }
    $td.append($ul);
}

function createFormulaHTML(formulaAsJson, isMainFormula = true) {
    switch (formulaAsJson.type) {
        case 'litteral':
            return formulaAsJson.value;

        case 'neutral':
            return NEUTRAL_ELEMENTS[formulaAsJson.value];

        case 'negation':
        case 'ofcourse':
        case 'whynot':
            return UNARY_OPERATORS[formulaAsJson.type] + createFormulaHTML(formulaAsJson.value, false);

        case 'orthogonal':
            return createFormulaHTML(formulaAsJson.value, false)
                + '<span class="exponent">⊥</span>';

        case 'implication':
        case 'conjunction':
        case 'disjunction':
        case 'tensor':
        case 'par':
        case 'with':
        case 'plus':
        case 'lollipop':
            let formula =
                '<span class="left-formula">'
                + createFormulaHTML(formulaAsJson['value1'], false)
                + '</span>'
                + addPrimaryOperator(BINARY_OPERATORS[formulaAsJson.type], isMainFormula)
                + '<span class="right-formula">'
                + createFormulaHTML(formulaAsJson['value2'], false)
                + '</span>';
            return addParentheses(formula, isMainFormula);

        default:
            console.error('No display rule for type ' + formulaAsJson.type);
            return '';
    }
}

function addPrimaryOperator(operator, isMainFormula) {
    if (isMainFormula) {
        return '<span class="primaryOperator">' + operator + '</span>';
    }

    return  operator;
}

function addParentheses(formula, isMainFormula) {
    if (!isMainFormula) {
        return '(' + formula + ')';
    }

    return  formula;
}

// ************
// PROOF UPDATE
// ************

function addSequentListPremisses($td, sequentList, rule) {
    // Add line
    $td.addClass('inference');

    // Add rule symbol
    $td.data('rule', rule);
    $td.next('.tagBox')
        .html($('<div>', {'class': 'tag'})
            .html(RULES[rule]));

    // Remove old premisses if any
    let $table = $td.closest('table');
    $table.prevAll().each(function (i, e) {
        e.remove();
    });

    // Mark proof as incomplete
    markAsIncomplete();

    // Add new sequents
    if (sequentList.length === 0) {
        checkProofIsComplete();
    } else if (sequentList.length === 1) {
        createSequent(sequentList[0]).insertBefore($table);
    } else {
        let $div = $('<div>');
        for (let i = 0; i < sequentList.length; i++) {
            let $sibling = $('<div>', {'class': 'sibling'})
            $sibling.append(createSequent(sequentList[i]))
            $div.append($sibling);
        }
        $div.insertBefore($table);
    }
}

// **********
// OPERATIONS
// **********

function getRules(formulaAsJson) {
    switch (formulaAsJson.type) {
        case 'litteral':
        case 'orthogonal':
            return [{'event': 'click', 'element': 'main-formula', 'rule': 'axiom'}];

        case 'tensor':
        case 'par':
        case 'with':
            return [{'event': 'click', 'element': 'main-formula', 'rule': formulaAsJson.type}];

        case 'plus':
            return [
                {'event': 'click', 'element': 'left-formula', 'rule': 'plus_left'},
                {'event': 'click', 'element': 'right-formula', 'rule': 'plus_right'}
            ];

        case 'neutral':
            switch (formulaAsJson.value) {
                case 'one':
                case 'top':
                case 'bottom':
                case 'zero': // click on zero will display a pedagogic error
                    return [{'event': 'click', 'element': 'main-formula', 'rule': formulaAsJson.value}];

                default:
                    return [];
            }

        default:
            return [];
    }
}

function buildApplyRuleCallBack(rule, $li) {
    return function() {
        let $td = $li.closest('td');
        let formulaPosition = $li.parent().children().index($li);

        applyRule(rule, $td, formulaPosition);
    }
}

function applyRule(rule, $td, formulaPosition) {
    let newSequent = getSequentWithPermutations($td);

    $.ajax({
        type: 'POST',
        url: '/apply_rule',
        contentType:'application/json; charset=utf-8',
        data: JSON.stringify({
            'rule': rule,
            'sequent': newSequent,
            'formulaPosition': formulaPosition
        }),
        success: function(data)
        {
            console.log(data);
            if (data.success === true) {
                cleanPedagogicError();
                addSequentListPremisses($td, data['sequentList'], rule);
            } else {
                displayPedagogicError(data['errorMessage']);
            }
        },
        error: function(jqXHR, textStatus, errorThrown) {
            console.log(jqXHR);
            console.log(jqXHR.responseText);
            console.log(textStatus);
            console.log(errorThrown);
            alert('Technical error, check browser console for more details.');
        }
    });
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

// ************
// GLOBAL PROOF
// ************

function checkProofIsComplete() {
    let $mainDiv = $('#main-proof-container')
        .children('div');
    let $mainTable = $mainDiv.children('div.proof')
        .children('table').last();
    if (recCheckIsComplete(recGetProofAsJson($mainTable))) {
        $mainDiv.removeClass('proofIsIncomplete');
        $mainDiv.addClass('proofIsDone');
    }
}

function recGetProofAsJson($table) {
    let $td = $table.children('td.sequent');
    let sequent = $td.data('sequent');
    let rule = $td.data('rule') || null;
    let premisses = [];
    if (rule !== null) {
        let $prev = $table.prev();
        if ($prev.length) {
            if ($prev.prop('tagName') === 'TABLE') {
                premisses = [recGetProofAsJson($prev)];
            } else {
                $prev.children('div.sibling').each(function (i, sibling) {
                    premisses.push(recGetProofAsJson($(sibling).children('table')));
                })
            }
        }
    }

    return {
        'sequent': sequent,
        'rule': rule,
        'premisses': premisses
    }
}

function recCheckIsComplete(proofAsJson) {
    if (proofAsJson.rule === null) {
        return false;
    }

    let response = true;

    for (let i = 0; i < proofAsJson.premisses.length; i++) {
        response = response && recCheckIsComplete(proofAsJson.premisses[i]);
    }

    return response;
}

function markAsIncomplete() {
    let $mainDiv = $('#main-proof-container')
        .children('div');
    $mainDiv.removeClass('proofIsDone');
    $mainDiv.addClass('proofIsIncomplete');
}

// *****
// POPUP
// *****

function openpopupoptions() {
    $('#dialog-form-options').dialog( 'open' );
}

function openpopuptext() {
    $('#dialog-form-text').dialog( 'open' );
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