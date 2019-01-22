import React from 'react';
import PropTypes from 'prop-types';
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import IconButton from '@material-ui/core/IconButton';
import Typography from '@material-ui/core/Typography';
import InputBase from '@material-ui/core/InputBase';
import Badge from '@material-ui/core/Badge';
import MenuItem from '@material-ui/core/MenuItem';
import Menu from '@material-ui/core/Menu';
import { fade } from '@material-ui/core/styles/colorManipulator';
import { withStyles } from '@material-ui/core/styles';
import MenuIcon from '@material-ui/icons/Menu';
import SearchIcon from '@material-ui/icons/Search';
import AccountCircle from '@material-ui/icons/AccountCircle';
import MailIcon from '@material-ui/icons/Mail';
import NotificationsIcon from '@material-ui/icons/Notifications';
import MoreIcon from '@material-ui/icons/MoreVert';

// https://github.com/mui-org/material-ui/blob/master/docs/src/pages/demos/app-bar/PrimarySearchAppBar.hooks.js

const primary = "#ff0000"; // #F44336

const styles = (theme :any) => ({
    root: {
        width: '100%',
    },
    sectionIcons: {
        display: 'none',
        [theme.breakpoints.up('md')]: {
            display: 'flex',
        },
    },
    grow: {
        flexGrow: 1,
    },
});

interface IProps  {
    classes: any
}
interface IState {
    anchorEl?: any;
    isMenuOpen: boolean;
}

class NavBar extends React.Component<IProps, IState> {

    constructor(props: any){
            super(props);
            this.state = {
                anchorEl: null,
                isMenuOpen: false
            };
        }


  handleProfileMenuOpen = (event :any) => {
    this.setState({ anchorEl: event.currentTarget, isMenuOpen: true });
  };

  handleProfileMenuClose = () => {
    this.setState({ anchorEl: null, isMenuOpen: false });
  }

  render() {
    const { anchorEl, isMenuOpen } = this.state;
    const { classes } = this.props;


    const renderBrand = (
        <Typography variant="h6" color="inherit" noWrap className={classes.grow}>
            Infomark
        </Typography>
    );

    const renderMenu = (
        <Menu
            anchorEl={anchorEl}
            open={isMenuOpen}
            onClose={this.handleProfileMenuClose}
            anchorOrigin={{ vertical: 'top', horizontal: 'right' }}
            transformOrigin={{ vertical: 'top', horizontal: 'right' }}
        >
            <MenuItem>My account</MenuItem>
            <MenuItem>Logout</MenuItem>
      </Menu>
    );

    const renderIcons = (
        <div>
        <div className={classes.root}>
            <div className={classes.sectionIcons}>
                <IconButton color="inherit">
                    <Badge badgeContent={4} color="secondary">
                        <MailIcon />
                    </Badge>
                </IconButton>
                <IconButton color="inherit">
                    <Badge badgeContent={17} color="secondary">
                        <NotificationsIcon />
                    </Badge>
                </IconButton>
                <IconButton
                    aria-owns={'material-appbar'}
                    aria-haspopup={"false"}
                    color="inherit"
                    onClick={this.handleProfileMenuOpen}
                >
                    <AccountCircle />
                </IconButton>
            </div>
        </div>
        </div>
    );


    return (
      <div>
        <AppBar position="static">
          <Toolbar>
            {renderBrand}
            {renderIcons}
          </Toolbar>
        </AppBar>
        {renderMenu}
      </div>
    );
  }
}

export default withStyles(styles)(NavBar);
