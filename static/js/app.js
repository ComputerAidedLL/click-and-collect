$( function() {
    let $sequentForm = $('#sequent-form');
    $sequentForm.on('submit', function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    // Parse URL and auto-complete / auto-submit sequent form
    let searchParams = new URLSearchParams(window.location.search);
    if (searchParams.has('s')) {
        $sequentForm.find($('input[name=sequentAsString]')).val(searchParams.get('s'));
        submitSequent($sequentForm);
    }

    // Create tutorial proof
    $('.tutorial .proof-container').each(function (i, container) {
        let $container = $(container);
        let sequentAsJson = JSON.parse($container.html());
        $container.html('');
        initProofWithSequent(sequentAsJson, $container, true);
    })

    // Create rules proof
    $('.rules .proof-container').each(function (i, container) {
        let $container = $(container);
        let proofAsJson = JSON.parse($container.html());
        $container.html('');
        initProof(proofAsJson, $container, false);
    })
} );

// ************
// SEQUENT FORM
// ************

function submitSequent(element) {
    cleanMainProof();

    let form = $(element).closest('form');
    let sequentAsString = form.find($('input[name=sequentAsString]')).val();

    // We update current URL by adding sequent in query parameters
    let currentUrl = new URL(window.location.href);
    currentUrl.searchParams.set('s', sequentAsString.toString());
    window.history.pushState(sequentAsString, "Linear logic proof start", currentUrl.toString());

    parseSequentAsString(sequentAsString, $('#main-proof-container'));
}

function parseSequentAsString(sequentAsString, $container) {
    let apiUrl = '/parse_sequent';

    $.ajax({
        type: 'GET',
        url: apiUrl,
        data: { sequentAsString },
        success: function(data)
        {
            if (data['is_valid']) {
                initProofWithSequent(data['sequent_as_json'], $container, true);
            } else {
                displayPedagogicError(data['error_message'], $container);
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

function cleanMainProof() {
    $('#main-proof-container').html('');
}

