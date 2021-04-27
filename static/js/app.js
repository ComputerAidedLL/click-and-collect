$( function() {
    let $sequentForm = $('#sequent-form');
    $sequentForm.on('submit', function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    // Parse URL and auto-complete / auto-submit sequent form
    let sequentParam = getQueryParamInUrl('s');
    if (sequentParam !== null) {
        $sequentForm.find($('input[name=sequentAsString]')).val(sequentParam);
        submitSequent($sequentForm);
    }
    
    // Parse URL hash
    switch (window.location.hash) {
        case '#tutorial':
            showTutorial();
            break;

        case '#rules':
            showRules();
            break;
    }
} );

// ************
// SEQUENT FORM
// ************

function submitSequent(element) {
    cleanMainProof();

    let form = $(element).closest('form');
    let sequentAsString = form.find($('input[name=sequentAsString]')).val();

    // We update current URL by adding sequent in query parameters
    addQueryParamInUrl('s', sequentAsString.toString(), 'Linear logic proof start');
    // We get autoReverse option in URL
    let autoReverse = getQueryParamInUrl('auto_reverse') === '1';

    parseSequentAsString(sequentAsString, $('#main-proof-container'), autoReverse);
}

function parseSequentAsString(sequentAsString, $container, autoReverse) {
    let apiUrl = '/parse_sequent';

    $.ajax({
        type: 'GET',
        url: apiUrl,
        data: { sequentAsString },
        success: function(data)
        {
            if (data['is_valid']) {
                initProof(data['proof'], $container, {
                    withInteraction: true,
                    exportButtons: true,
                    checkProvability: true,
                    autoReverseOption: true,
                    autoReverse: autoReverse,
                    onAutoReverseToggle: onAutoReverseToggle
                });
            } else {
                displayPedagogicError(data['error_message'], $container);
            }
        },
        error: onAjaxError
    });
}

function cleanMainProof() {
    $('#main-proof-container').html('');
}

// ********
// TUTORIAL
// ********

function showTutorial() {
    cleanUrlParams('Show tutorial');

    let $tutorial = $('.tutorial');
    if ($tutorial.data('init') !== true) {
        // Create tutorial proof
        $('.tutorial .proof-container').each(function (i, container) {
            let $container = $(container);
            let proof = JSON.parse(uncompressJson($container.html()));
            $container.html('');
            initProof(proof, $container, {
                withInteraction: true
            });
        })

        $tutorial.data('init', true);
    }

    $tutorial.removeClass('hidden');
}

function hideTutorial() {
    $('.tutorial').addClass('hidden');
    cleanUrlHash('Hide tutorial');
}

// *****
// RULES
// *****

function showRules() {
    cleanUrlParams('Show rules');

    let $rules = $('.rules');
    if ($rules.data('init') !== true) {
        // Create rules proof
        $('.rules .proof-container').each(function (i, container) {
            let $container = $(container);
            let proofAsJson = JSON.parse(uncompressJson($container.html()));
            $container.html('');
            initProof(proofAsJson, $container);
        })

        $rules.data('init', true);
    }
    $rules.removeClass('hidden');
}

function hideRules() {
    $('.rules').addClass('hidden');
    cleanUrlHash('Hide rules');
}

// *******************
// AUTO-REVERSE OPTION
// *******************

function onAutoReverseToggle(autoReverse) {
    if (autoReverse) {
        addQueryParamInUrl('auto_reverse', '1', 'Auto reverse mode set to true');
    } else {
        addQueryParamInUrl('auto_reverse', null, 'Auto reverse mode set to false');
    }
}

// *****
// UTILS
// *****

function addQueryParamInUrl (key, value, title) {
    let currentUrl = new URL(window.location.href);

    if (value !== null) {
        currentUrl.searchParams.set(key, value);
    } else {
        currentUrl.searchParams.delete(key);
    }
    currentUrl.hash = '';
    window.history.pushState(value, title, currentUrl.toString());
}

function getQueryParamInUrl (key) {
    let searchParams = new URLSearchParams(window.location.search);
    if (searchParams.has(key)) {
        return searchParams.get(key);
    }

    return null;
}

function cleanUrlHash (title) {
    let currentUrl = new URL(window.location.href);

    currentUrl.hash = '';
    window.history.pushState(null, title, currentUrl.toString());
}

function cleanUrlParams (title) {
    let currentUrl = new URL(window.location.href);

    currentUrl.searchParams.forEach(function (value, key) {
        currentUrl.searchParams.delete(key);
    })

    window.history.pushState(null, title, currentUrl.toString());
}