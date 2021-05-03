// **************
// DISPLAY CONFIG
// **************

const UNARY_CONNECTORS = {
    'negation': '¬',
    'ofcourse': '!',
    'whynot': '?'
};

const BINARY_CONNECTORS = {
    'implication': '<span class="binary-connector">→</span>',
    'conjunction': '<span class="binary-connector">∧</span>',
    'disjunction': '<span class="binary-connector">∨</span>',
    'tensor': '<span class="binary-connector">⊗</span>',
    'par': '<span class="binary-connector flip">&</span>',
    'with': '<span class="binary-connector">&</span>',
    'plus': '<span class="binary-connector">⊕</span>',
    'lollipop': '<span class="binary-connector">⊸</span>'
};

const NEUTRAL_ELEMENTS = {
    'true': 'true',
    'false': 'false',
    'one': '1',
    'bottom': '⊥',
    'top': '⊤',
    'zero': '0'
};

// ***************
// DISPLAY SEQUENT
// ***************

function createSequent(sequent, options) {
    let $sequentDiv = $('<div>', {'class': 'sequent'})
        .data('sequentWithoutPermutation', sequent);

    if ('hyp' in sequent) {
        createFormulaList(sequent, 'hyp', $sequentDiv, options);
    }

    let $thesisSpan = $('<span class="turnstile">⊢</span>');
    if (options.withInteraction) {
        $thesisSpan.addClass('clickable');
        addClickAndDoubleClickEvent($thesisSpan, function () {
            applyRule({rule: 'axiom'}, $sequentDiv);
        }, function () {
            autoProveSequent($sequentDiv);
        });
    }
    $sequentDiv.append($thesisSpan);

    if ('cons' in sequent) {
        createFormulaList(sequent, 'cons', $sequentDiv, options);
    }

    return $sequentDiv;
}

function createFormulaList(sequent, sequentPart, $sequentDiv, options) {
    let $ul = $('<ul>', {'class': ['commaList ' + sequentPart]});

    if (options.withInteraction) {
        $ul.sortable({
            helper : 'clone',
            axis: 'x',
            opacity: 0.2,
            start: function(e, ui){
                ui.placeholder.width(ui.item.width());
            }
        });
    }

    for (let i = 0; i < sequent[sequentPart].length; i++) {
        let formulaAsJson = sequent[sequentPart][i];
        let $li = $('<li>').data('initialPosition', i);

        // Build formula
        let $span = $('<span>', {'class': 'main-formula'})
            .html(createFormulaHTML(formulaAsJson, true));
        $li.append($span);

        if (options.withInteraction) {
            // Add events (click, double-click), and classes for hover
            addEventsAndStyle($li, formulaAsJson);
        }

        $ul.append($li);
    }

    $sequentDiv.append($ul);
}

function createFormulaHTML(formulaAsJson, isMainFormula = true) {
    switch (formulaAsJson.type) {
        case 'litt':
            return formulaAsJson.value.replace(/\d+/, digits => `<sub>${digits}</sub>`);

        case 'one':
        case 'bottom':
        case 'top':
        case 'zero':
            let neutralElement = NEUTRAL_ELEMENTS[formulaAsJson.type];
            if (isMainFormula) {
                return `<span class="primaryConnector">${neutralElement}</span>`;
            }
            return neutralElement;

        case 'negation':
            return UNARY_CONNECTORS[formulaAsJson.type] + createFormulaHTML(formulaAsJson.value, false);

        case 'ofcourse':
        case 'whynot':
            let unaryConnector = UNARY_CONNECTORS[formulaAsJson.type];
            let subFormula = createFormulaHTML(formulaAsJson.value, false);
            if (isMainFormula) {
                unaryConnector = `<span class="primaryConnector">${unaryConnector}</span>`;
                subFormula = `<span class="sub-formula">${subFormula}</span>`;
            }
            return unaryConnector + subFormula;

        case 'dual':
            return createFormulaHTML(formulaAsJson.value, false)
                + '<sup>⊥</sup>';

        case 'implication':
        case 'conjunction':
        case 'disjunction':
        case 'tensor':
        case 'par':
        case 'with':
        case 'plus':
        case 'lollipop':
            let connector = BINARY_CONNECTORS[formulaAsJson.type];
            if (isMainFormula) {
                connector = `<span class="primaryConnector">${connector}</span>`;
            }

            let leftFormula = createFormulaHTML(formulaAsJson['value1'], false);
            let rightFormula = createFormulaHTML(formulaAsJson['value2'], false);
            if (isMainFormula) {
                leftFormula = `<span class="left-formula">${leftFormula}</span>`;
                rightFormula = `<span class="right-formula">${rightFormula}</span>`;
            }
            let formula = leftFormula + connector + rightFormula;

            if (!isMainFormula) {
                return `(${formula})`;
            }

            return formula;

        default:
            console.error('No display rule for type ' + formulaAsJson.type);
            return '';
    }
}

// *****
// RULES
// *****

function getRules(formulaAsJson) {
    switch (formulaAsJson.type) {
        case 'litt':
        case 'dual':
            return [{'element': 'main-formula', 'onclick': [{'rule': 'axiom', 'needPosition': false}]}];

        case 'tensor':
        case 'par':
        case 'with':
            return [{'element': 'main-formula', 'onclick': [{'rule': formulaAsJson.type, 'needPosition': true}]}];

        case 'plus':
            return [
                {'element': 'left-formula', 'onclick': [{'rule': 'plus_left', 'needPosition': true}]},
                {'element': 'right-formula', 'onclick': [{'rule': 'plus_right', 'needPosition': true}]}
            ];

        case 'one':
        case 'zero': // click on zero will display a pedagogic error
            return [{'element': 'main-formula', 'onclick': [{'rule': formulaAsJson.type, 'needPosition': false}]}];

        case 'top':
        case 'bottom':
            return [{'element': 'main-formula', 'onclick': [{'rule': formulaAsJson.type, 'needPosition': true}]}];

        case 'ofcourse':
            return [{'element': 'main-formula', 'onclick': [{'rule': 'promotion', 'needPosition': true}]}];

        case 'whynot':
            return [
                {'element': 'primaryConnector', 'onclick': [
                    {'rule': 'weakening', 'needPosition': true},
                    {'rule': 'contraction', 'needPosition': true}
                ]},
                {'element': 'sub-formula', 'onclick': [
                    {'rule': 'dereliction', 'needPosition': true},
                    {'rule': 'contraction', 'needPosition': true}
                ]}
            ];

        default:
            return [];
    }
}

function addEventsAndStyle($li, formulaAsJson) {
    $li.find('span.' + 'main-formula').first().addClass('hoverable');

    let rules = getRules(formulaAsJson);
    for (let ruleEvent of rules) {
        let $spanForEvent = $li.find('span.' + ruleEvent.element).first();

        // Some hover config
        $spanForEvent.addClass('clickable');
        if (ruleEvent.element !== 'main-formula') {
            $spanForEvent.addClass('highlightableExpr');
        }

        // Some hover config for tensor
        if (ruleEvent.onclick[0] === 'tensor') {
            $li.find('span' + '.left-formula').first().addClass('tensor-left');
            $li.find('span' + '.right-formula').first().addClass('tensor-right');
        }

        // Add click and double click events
        if (ruleEvent.onclick.length === 1) {
            // Single click
            $spanForEvent.on('click', buildApplyRuleCallBack(ruleEvent.onclick[0], $li));
        } else {
            // Single click AND Double click event
            let singleClickCallBack = buildApplyRuleCallBack(ruleEvent.onclick[0], $li);
            let doubleClickCallBack = buildApplyRuleCallBack(ruleEvent.onclick[1], $li);

            addClickAndDoubleClickEvent($spanForEvent, singleClickCallBack, doubleClickCallBack);
        }
    }
}

function buildApplyRuleCallBack(ruleConfig, $li) {
    return function() {
        let $sequentDiv = $li.closest('div.sequent');
        let ruleRequest = {rule: ruleConfig.rule};

        if (ruleConfig.needPosition) {
            ruleRequest['formulaPosition'] = $li.parent().children().index($li);
        }

        applyRule(ruleRequest, $sequentDiv);
    }
}

// ******************
// DOUBLE CLICK EVENT
// ******************

const CLICK_DELAY = 200;
window.clickCount = 0;
window.clickTimer = null;

function addClickAndDoubleClickEvent ($element, singleClickCallBack, doubleClickCallBack) {
    // https://stackoverflow.com/a/7845282
    $element.on('click', function () {
        clickCount++;
        if (clickCount === 1) {
            window.clickTimer = setTimeout(function () {
                singleClickCallBack();
                window.clickCount = 0;
            }, CLICK_DELAY);
        } else {
            clearTimeout(window.clickTimer);
            doubleClickCallBack();
            window.clickCount = 0;
        }
    })
}

// *******************
// FORMULA PERMUTATION
// *******************

function getSequentPermutation($sequentDiv) {
    return {
        'hyp': getFormulasPermutation($sequentDiv.find('ul.hyp')),
        'cons': getFormulasPermutation($sequentDiv.find('ul.cons'))
    };
}

function getFormulasPermutation($ul) {
    let permutation = [];

    $ul.find('li').each(function(i, obj) {
        let initialPosition = $(obj).data('initialPosition');
        permutation.push(initialPosition);
    })

    return permutation;
}

function permuteSequent(sequentWithoutPermutation, sequentPermutation) {
    return {
        'hyp': permuteFormulas(sequentWithoutPermutation['hyp'], sequentPermutation['hyp']),
        'cons': permuteFormulas(sequentWithoutPermutation['cons'], sequentPermutation['cons'])
    };
}

function permuteFormulas(formulasWithoutPermutation, formulasPermutation) {
    let newFormulas = [];

    for (let initialPosition of formulasPermutation) {
        newFormulas.push(formulasWithoutPermutation[initialPosition]);
    }

    return newFormulas;
}

// ******************
// AUTO-PROVE SEQUENT
// ******************

function autoProveSequent($sequentDiv) {
    if ($sequentDiv.data('notProvable') === true || $sequentDiv.data('notAutoProvable') === true) {
        return;
    }

    let $container = $sequentDiv.closest('.proof-container');
    let options = $container.data('options');

    // Sequent json that was stored in div may have been permuted before rule applying
    let sequentWithoutPermutation = $sequentDiv.data('sequentWithoutPermutation');
    let permutationBeforeRule = getSequentPermutation($sequentDiv);
    let sequent = permuteSequent(sequentWithoutPermutation, permutationBeforeRule);

    $.ajax({
        type: 'POST',
        url: '/auto_prove_sequent',
        contentType:'application/json; charset=utf-8',
        data: compressJson(JSON.stringify(sequent)),
        success: function(data)
        {
            if (data.success) {
                cleanPedagogicError($container);
                let $sequentContainer = removeSequentDiv($sequentDiv);
                createSubProof(data['proof'], $sequentContainer, options);
                markAsCompleteIfProofIsComplete($container);
            } else {
                if (data['is_provable']) {
                    markAsNotAutoProvable($sequentDiv);
                } else {
                    markAsNotProvable($sequentDiv);
                }
            }
        },
        error: onAjaxError
    });
}

function markAsNotAutoProvable($sequentDiv) {
    $sequentDiv.data('notAutoProvable', true);
    let $turnstile = $sequentDiv.find('span.turnstile');
    $turnstile.addClass('not-auto-provable');
    $turnstile.attr('title', 'The automatic prover did not make it on this sequent');
}

function undoMarkAsNotAutoProvable($sequentDiv) {
    $sequentDiv.data('notAutoProvable', null);
    let $turnstile = $sequentDiv.find('span.turnstile');
    $turnstile.removeClass('not-auto-provable');
    $turnstile.removeAttr('title');
}