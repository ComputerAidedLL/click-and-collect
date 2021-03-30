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

// ****************
// DISPLAY FUNCTION
// ****************

function createSequent(sequentAsJson) {
    let $sequentDiv = $('<div>', {'class': 'sequent'})
        .data('sequentWithoutPermutation', sequentAsJson);

    if ('hyp' in sequentAsJson) {
        createFormulaList(sequentAsJson, 'hyp', $sequentDiv);
    }

    let $thesisSpan = $('<span class="turnstile explained">⊢</span>');
    $thesisSpan.on('click', function () {
        applyRule('axiom', $sequentDiv, []);
    });
    $sequentDiv.append($thesisSpan);

    if ('cons' in sequentAsJson) {
        createFormulaList(sequentAsJson, 'cons', $sequentDiv);
    }

    return $sequentDiv;
}

function createFormulaList(sequentAsJson, field, $sequentDiv) {
    let $ul = $('<ul>', {'class': ['commaList ' + field]})
        .sortable({
            helper : 'clone',
            axis: 'x',
            opacity: 0.2,
            start: function(e, ui){
                ui.placeholder.width(ui.item.width());
            }
        });

    for (let i = 0; i < sequentAsJson[field].length; i++) {
        let formulaAsJson = sequentAsJson[field][i];
        let $li = $('<li>').data('initialPosition', i);

        // Build formula
        let $span = $('<span>', {'class': 'main-formula'})
            .html(createFormulaHTML(formulaAsJson, true));
        $li.append($span);

        // Add events (click, double-click), and classes for hover
        addEventsAndStyle($li, formulaAsJson);

        $ul.append($li);
    }

    $sequentDiv.append($ul);
}

function createFormulaHTML(formulaAsJson, isMainFormula = true) {
    switch (formulaAsJson.type) {
        case 'litteral':
            return `${formulaAsJson.value}`;

        case 'neutral':
            let neutralElement = NEUTRAL_ELEMENTS[formulaAsJson.value];
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

        case 'orthogonal':
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
        case 'litteral':
        case 'orthogonal':
            return [{'element': 'main-formula', 'onclick': ['axiom']}];

        case 'tensor':
        case 'par':
        case 'with':
            return [{'element': 'main-formula', 'onclick': [formulaAsJson.type]}];

        case 'plus':
            return [
                {'element': 'left-formula', 'onclick': ['plus_left']},
                {'element': 'right-formula', 'onclick': ['plus_right']}
            ];

        case 'neutral':
            switch (formulaAsJson.value) {
                case 'one':
                case 'top':
                case 'bottom':
                case 'zero': // click on zero will display a pedagogic error
                    return [{'element': 'main-formula', 'onclick': [formulaAsJson.value]}];

                default:
                    return [];
            }

        case 'ofcourse':
            return [{'element': 'main-formula', 'onclick': ['promotion']}];

        case 'whynot':
            return [
                {'element': 'primaryConnector', 'onclick': ['weakening', 'contraction']},
                {'element': 'sub-formula', 'onclick': ['dereliction', 'contraction']}
            ];

        default:
            return [];
    }
}

function addEventsAndStyle($li, formulaAsJson) {
    let rules = getRules(formulaAsJson);
    for (let ruleEvent of rules) {
        let $spanForEvent = $li.find('span.' + ruleEvent.element).first();

        // Some hover config
        $spanForEvent.addClass('clickableExpr');
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

function buildApplyRuleCallBack(rule, $li) {
    return function() {
        let $sequentDiv = $li.closest('div.sequent');
        let formulaPositions = [$li.parent().children().index($li)];

        applyRule(rule, $sequentDiv, formulaPositions);
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